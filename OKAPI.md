# Okapi

Scala 3 macro library that turns annotated controller classes into fully-wired Tapir endpoints served by ZIO HTTP — no boilerplate, no manual routing.

---

## How it works

At compile time, Okapi's macro inspects annotated classes and generates:

1. A list of Tapir `ZServerEndpoint` objects (path, method, inputs, outputs, server logic)
2. ZIO HTTP `Routes` ready to plug into `Server.serve`
3. An automatic `ZLayer` that resolves the entire dependency tree (controllers + services + repos)

No reflection at runtime. Everything is resolved during compilation.

---

## Project setup

```scala
// build.sbt
libraryDependencies += "io.github.andrzej-swietek" %% "okapi-core" % "0.1.3"
scalacOptions += "-Xmax-inlines:128"   // required for macro expansion depth
```

---

## Annotations

### Controller-level

| Annotation | Purpose |
|------------|---------|
| `@Controller("/base/path")` | Marks a class as a controller; sets the path prefix |
| `@Tag("GroupName")` | Groups endpoints under a Swagger tag |
| `@ApiTag("GroupName")` | Same as `@Tag`, but collision-free when `import zio.*` is in scope (it also exports `zio.Tag`) |

### Method-level (HTTP verbs)

| Annotation | HTTP method |
|------------|-------------|
| `@Get("/path")` | GET |
| `@Post("/path")` | POST |
| `@Put("/path")` | PUT |
| `@Delete("/path")` | DELETE |
| `@Patch("/path")` | PATCH |
| `@WebSocket("/path")` | WebSocket (GET upgrade) |

Path templates support `{paramName}` placeholders: `@Get("/{id}/reviews")` or `@WebSocket("/chat/{room}")`.

### Parameter-level

| Annotation | Maps to |
|------------|---------|
| `@Path("name")` | URL path segment — matched against `{name}` in template |
| `@Query("name")` | Query string parameter |
| `@Header("name")` | HTTP request header |
| `@Cookie("name")` | HTTP cookie |
| `@RequestBody` | Request body (dispatched by `@Consumes`) |
| `@BearerAuth` | Binds the `Authorization: Bearer <token>` header to a `String` parameter and advertises a bearer security scheme |

All parameter types support `Option[T]` for optional values.

### Documentation

| Annotation | Purpose |
|------------|---------|
| `@Summary("text")` | Short description shown in Swagger |
| `@Description("text")` | Long description shown in Swagger |
| `@Deprecated()` | Marks endpoint as deprecated in Swagger |

### Content type

| Annotation | Purpose |
|------------|---------|
| `@Consumes("media/type")` | Declares request body format (default: `application/json`) |
| `@Produces("media/type")` | Declares response format (default: `application/json`) |

### Response status

The success status is inferred: **201** for `@Post`, **204** for a `Unit` return, **200** otherwise. Override it explicitly:

| Annotation | Purpose |
|------------|---------|
| `@Status(code)` | Sets the success HTTP status code for the endpoint |

---

## Supported media types

### Request body (`@Consumes`)

| Media type | Parameter type | Tapir codec |
|------------|---------------|-------------|
| `application/json` (default) | any case class with `JsonCodec` | `jsonBody` |
| `application/x-www-form-urlencoded` | case class with `Codec[String, T, XWwwFormUrlencoded]` | `formBody` |
| `multipart/form-data` | case class with `MultipartCodec[T]` | `multipartBody` |
| `application/octet-stream` (or any, `Array[Byte]` param) | `Array[Byte]` | `byteArrayBody` |
| `text/html` | `String` | `htmlBodyUtf8` |
| `text/xml` / `application/xml` | `String` | string body with XML content-type |
| `text/javascript` / `application/javascript` | `String` | string body with JS content-type |
| `text/*` (other) | `String` | `stringBody` |

### Response body (`@Produces`)

| Media type | Return type | Tapir codec |
|------------|------------|-------------|
| `application/json` (default) | any case class with `JsonCodec` | `jsonBody` |
| `text/plain` | `String` or any case class | `stringBody` |
| `text/html` | `String` | `htmlBodyUtf8` |
| `text/xml` / `application/xml` | `String` | XML content-type |
| `text/javascript` / `application/javascript` | `String` | JS content-type |
| `text/event-stream` | `String` | SSE content-type |
| `text/*` (other) | `String` | `stringBody` |
| `application/octet-stream` | `Array[Byte]` | `byteArrayBody` |
| `image/*` | `Array[Byte]` | `byteArrayBody` |
| `application/pdf` | `Array[Byte]` | `byteArrayBody` |
| `application/zip` | `Array[Byte]` | `byteArrayBody` |
| `application/x-www-form-urlencoded` | case class with form codec | `formBody` |

---

## WebSocket support

Annotate a method with `@WebSocket("/path")` to create a WebSocket endpoint. The method must return `WsPipe[In, Out]` (a stream transformer).

Supported message types:

| `WsPipe` type | Wire format |
|---------------|-------------|
| `WsPipe[String, String]` | Text frames |
| `WsPipe[Array[Byte], Array[Byte]]` | Binary frames |

Return types for `@WebSocket`:

| Return type | Meaning |
|-------------|---------|
| `WsPipe[In, Out]` | Pure — always connected, no setup effect |
| `IO[ApiError, WsPipe[In, Out]]` | Effectful — runs setup on connect, then streams |

Path params (`@Path`), query params (`@Query`), and headers (`@Header`) are supported on WebSocket methods — they are resolved during the HTTP upgrade handshake.

```scala
@Controller("/api/ws")
@Tag("WebSocket")
final class WsController(service: SomeService) {

  @WebSocket("/echo")
  @Summary("Echo every message back")
  def echo: WsPipe[String, String] =
    _.map(msg => s"echo: $msg")

  @WebSocket("/chat/{room}")
  def chat(
    @Path("room")  room: String,
    @Query("user") user: Option[String],
  ): WsPipe[String, String] =
    _.map(msg => s"[$room] ${user.getOrElse("anon")}: $msg")

  @WebSocket("/updates")
  @Summary("Run setup on connect, then stream")
  def updates: IO[ApiError, WsPipe[String, String]] =
    service.initialData.map { data =>
      stream =>
        ZStream.succeed(s"connected count=${data.total}") ++
        stream.map(cmd => s"cmd=$cmd")
    }
}
```

`WsPipe[In, Out]` is a type alias for `ZStream[Any, Throwable, In] => ZStream[Any, Throwable, Out]`. Import `io.okapi.core.WsPipe` to use it. Import `zio.stream.ZStream` when constructing streams inside the method body.

---

## Return types (HTTP endpoints)

Controller methods can return:
- A pure value `T` — wrapped in `ZIO.succeed` automatically
- `IO[ApiError, T]` — error is mapped to the right HTTP status
- `ZIO[Any, ApiError, T]` — same as above, env must be `Any`
- `FileResponse` — binary download; sets `Content-Disposition` from the filename (quotes/CR/LF stripped)
- `ZStream[Any, Throwable, Byte]` (with `@Produces("application/octet-stream")`) — chunked binary streaming

`ApiError` variants:

| Case | HTTP status |
|------|-------------|
| `ApiError.BadRequest(msg)` | 400 |
| `ApiError.Unauthorized(msg)` | 401 |
| `ApiError.Forbidden(msg)` | 403 |
| `ApiError.NotFound(msg)` | 404 |
| `ApiError.Conflict(msg)` | 409 |
| `ApiError.UnprocessableEntity(msg)` | 422 |
| `ApiError.TooManyRequests(msg)` | 429 |
| `ApiError.Internal(msg)` | 500 |

---

## Okapi API

```scala
// Generate ZIO HTTP routes for controllers
Okapi.routes[Controllers]                           // Routes[Env, Response]

// Generate Swagger UI routes
Okapi.swagger[Controllers]("Title", "1.0.0")        // Routes[Any, Response]

// Render the OpenAPI 3 spec as a YAML string (serve at e.g. /openapi.yaml)
Okapi.openApiYaml[Controllers]("Title", "1.0.0")    // String

// Generate Tapir endpoint list (for custom handling)
Okapi.endpoints[MyController]                       // List[ZServerEndpoint[T, WS]]

// Auto-wire all dependencies into a single ZLayer
Okapi.autoLayer[Controllers]                        // ZLayer[Any, Nothing, Env]

// Manual layer registration (older API)
Okapi.registerOkapiControllers[Controllers](effect)
```

`Controllers` is a tuple type: `(BookController, UserController, AdminController)`.

---

## autoLayer

`Okapi.autoLayer[Controllers]` is a compile-time macro that:

1. Reads the primary constructor of each controller type
2. For each constructor parameter (service), recursively reads its constructor
3. Builds a topologically-sorted list of all required types (repos → services → controllers)
4. Generates `ZLayer.make[C1 & C2 & ...]` with `ZLayer.derive[T]` for every discovered type

Requirements for `autoLayer` to work:
- Every class in the dependency tree must be a plain class (`final class`)
- Dependencies are constructor parameters (no ZIO `Ref`, no `using`/`given` params)
- All classes must be discoverable at compile time (no dynamic dispatch)

Example:
```scala
// Instead of:
Server.serve(app).provide(
  ZLayer.succeed(serverConfig),
  Server.live,
  ZLayer.derive[BookController],
  ZLayer.derive[BookService],
  ZLayer.derive[BookRepository],
  ZLayer.derive[NotificationService],
  // ... everything manually listed
)

// Just:
Server.serve(app).provide(
  ZLayer.succeed(serverConfig),
  Server.live,
  Okapi.autoLayer[Controllers],
)
```

---

## Full example

```scala
@Controller("/api/books")
@Tag("Books")
final class BookController(bookService: BookService) {

  @Get("")
  @Summary("List books")
  def listBooks(
    @Query("genre") genre: Option[String],
    @Query("limit") limit: Option[Int],
  ): IO[ApiError, List[Book]] =
    bookService.listBooks(genre, limit.getOrElse(50))

  @Get("/{id}")
  @Summary("Get book by ID")
  def getBook(@Path("id") id: Int): IO[ApiError, Book] =
    bookService.getBook(id)

  @Post("")
  @Consumes("application/json")
  def createBook(@RequestBody body: CreateBookRequest): IO[ApiError, Book] =
    bookService.createBook(body)

  @Get("/{id}/reviews")
  def getReviews(@Path("id") id: Int): IO[ApiError, List[BookReview]] =
    bookService.getReviews(id)

  @Get("/export")
  @Produces("application/octet-stream")
  def exportCsv: IO[ApiError, Array[Byte]] =
    bookService.exportCsv
}

object Main extends ZIOAppDefault {
  private type Controllers = (BookController, UserController, AdminController)

  private lazy val app =
    Okapi.routes[Controllers] ++ Okapi.swagger[Controllers]("My API", "1.0")

  override def run =
    Server.serve(app).provide(
      ZLayer.succeed(Server.Config.default.port(8080)),
      Server.live,
      Okapi.autoLayer[Controllers],
    )
}
```

---

## Known limitations / TODO

### Missing features

- **`multipart/form-data` with auto-derived codec** — `Expr.summon[MultipartCodec[T]]` at macro expansion time fails when the codec is provided only via `sttp.tapir.generic.auto.*` wildcard import and the case class is defined in another file. Workaround: define `given MultipartCodec[T]` explicitly in the companion object of the form class.

- **Multiple response bodies (`oneOf`)** — Tapir supports `oneOf` for different response types per status code. Okapi always maps a single return type to a single output body.

- **Security schemes beyond bearer** — `@BearerAuth` covers bearer tokens (passed to the controller and advertised in OpenAPI). API-key / OAuth2 `securityIn` schemes and the `serverSecurityLogic` split are not generated; use `@Header` for those for now.

- **Request validation** — no `@Min`, `@Max`, `@NotBlank` etc. Input validation must be done in the controller/service body.

- **Server-Sent Events** — the `text/event-stream` codec is wired, but first-class streaming of `ServerSentEvent` values is not. Binary HTTP streaming (a `ZStream[Any, Throwable, Byte]` response body) **is** supported.

### Known gotchas

- `routes[Controllers]` and `swagger[Controllers]` must be `lazy val` (not `val`) in `object Main` to avoid JVM `Method too large` error when there are many endpoints.

- `@Tag` from `io.okapi.core.annotations` collides with `zio.Tag` under `import zio.*`. Either use `import zio.{ IO, ZIO }`, or use the **`@ApiTag`** alias, which never collides.

- `java.lang.System.currentTimeMillis()` — `import zio.*` shadows `System` with `zio.System`. Use the fully qualified name.

- After changing `core/`, run `sbt publishLocal` from the project root before compiling `okapi-example/`.
