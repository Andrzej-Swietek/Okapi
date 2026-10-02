# Okapi

Scala 3 macro library that turns annotated controller classes into fully-wired Tapir endpoints served by ZIO HTTP — no boilerplate, no manual routing. The core is effect-agnostic: ZIO is one backend (`okapi-zio`), and any `F[_]` with an `OkapiEffect[F]` works with `okapi-core` alone (see [Effect-agnostic core](#effect-agnostic-core)).

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
libraryDependencies += "io.github.andrzej-swietek" %% "okapi-zio" % "0.2.0"   // or "okapi-core" without ZIO
scalacOptions += "-Xmax-inlines:128"   // required for macro expansion depth
```

| Module | Contents | Depends on ZIO |
|--------|----------|----------------|
| `okapi-core` | annotations, macros, runtime, `JsoniterCodec`, `ApiResponse` | no |
| `okapi-openapi` | OpenAPI JSON/YAML documents and Swagger UI endpoints | no |
| `okapi-metrics` | endpoint metrics: a per-request callback and Prometheus | no |
| `okapi-client` | HTTP clients generated from annotated API traits | no |
| `sbt-okapi` | sbt plugin: writes the OpenAPI document and generates a `<name>-client` module | — |
| `okapi-zio` | ZIO HTTP routes, `ZStream` / SSE / WebSocket bodies, `ZLayer` wiring (brings `okapi-core` and `okapi-openapi`) | yes |

Requires Scala 3.6+.

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

`@Query`, `@Header` and `@Cookie` parameters are optional when typed `Option[T]` or given a default value (see
[Parameters](#parameters)); `@Path` parameters are always required.

### Documentation

| Annotation | Purpose |
|------------|---------|
| `@Summary("text")` | Short description shown in Swagger |
| `@Description("text")` | Long description shown in Swagger |
| `@Deprecated()` | Marks endpoint as deprecated in Swagger (`@java.lang.Deprecated` works too) |

### Content type

| Annotation | Purpose |
|------------|---------|
| `@Consumes("media/type")` | Request body media type (default depends on the body type, see [Supported media types](#supported-media-types)) |
| `@Produces("media/type")` | Response media type (default depends on the result type) |

### Response status

The success status is inferred: **204** for a `Unit` result, otherwise **201** for `@Post` and **200** for the other
verbs. Override it explicitly:

| Annotation | Purpose |
|------------|---------|
| `@Status(code)` | Sets the success HTTP status code for the endpoint |

---

## Supported media types

A declared `@Consumes` / `@Produces` is used **exactly** as the body's `Content-Type` (it is validated at compile
time). Without one, each body kind has its default.

### Request body (`@Consumes`)

| Body type | Media type | Body |
|-----------|-----------|------|
| case class | none or `application/json` (or `*+json`) | JSON — see [JSON](#json) |
| case class | `application/x-www-form-urlencoded` | form (`Codec[String, T, XWwwFormUrlencoded]`) |
| case class | `multipart/form-data` | multipart (`MultipartCodec[T]`) |
| `String` | none → `text/plain`; otherwise the declared one (`text/html`, `application/xml`, `application/json`, ...) | raw string |
| `Array[Byte]` | none → `application/octet-stream`; otherwise the declared one | raw bytes |
| `ZStream[Any, Throwable, Byte]` (okapi-zio) | — | binary stream |

Any other combination (e.g. a case class with `@Consumes("text/csv")`) is a compile error.

### Response body (`@Produces`)

| Result type | Media type | Body |
|-------------|-----------|------|
| case class | none or `application/json` (or `*+json`) | JSON — see [JSON](#json) |
| case class | `application/x-www-form-urlencoded` | form |
| `String` | none → `text/plain`; otherwise the declared one (`text/html`, `text/event-stream`, `application/json`, ...) | raw string |
| `Array[Byte]` | none → `application/octet-stream`; otherwise the declared one (`image/png`, `application/pdf`, ...) | raw bytes |
| `FileResponse` | as for `Array[Byte]` | bytes + `Content-Disposition` |
| `Unit` | — | empty, `204` |
| `ZStream[Any, Throwable, Byte]` (okapi-zio) | — | binary stream |

A case class with a text or binary media type (e.g. `@Produces("text/plain") def x: Book`) is a compile error:
return a `String` / `Array[Byte]` instead.

---

## JSON

The JSON codec of a body type `T` is the first found of:

1. a **Tapir JSON codec** (`Codec[String, T, CodecFormat.Json]`) in scope where the endpoints are generated — any
   Tapir JSON integration works, e.g. `import sttp.tapir.json.circe.*` or `import sttp.tapir.json.zio.*`;
2. a **jsoniter-scala** `JsonValueCodec[T]` — Okapi's default JSON library:
   ```scala
   import io.okapi.core.json.JsoniterCodec

   final case class Book(id: Int, title: String) derives JsoniterCodec
   ```
   **Sealed hierarchies and enums** work the same way and carry their case name in a `"type"` field
   (`JsoniterCodec.Discriminator`) — in the JSON *and* in the OpenAPI schema, so the docs match what is sent:
   ```scala
   sealed trait Payment derives JsoniterCodec            // {"type": "Card", "number": "4111"}
   final case class Card(number: String) extends Payment
   case object Cash extends Payment                      // {"type": "Cash"}
   enum Color derives JsoniterCodec { case Red, Green }  // {"type": "Red"}
   ```
   Or, with a custom configuration, `given JsonValueCodec[Book] = JsonCodecMaker.make(config)` (then provide a
   matching `Schema` if the format differs from the default). Containers of such
   types (`List[Book]`, `Option[Book]`, `Map[String, Book]`, ...) need no codec of their own: Okapi makes one;
3. with **okapi-zio**, a zio-json `JsonCodec[T]` (`derives JsonCodec`) — so existing ZIO code keeps working.

The OpenAPI schema comes with the codec: a `JsoniterCodec` carries its own; for a plain `JsonValueCodec` or a
zio-json codec it is a `Schema[T]` in scope, else derived for case classes, sealed hierarchies and enums.
Ambiguous codecs are a compile error. The error body (`{"code": ..., "message": ...}`) uses Okapi's own codec,
independent of the library used for other bodies.

---

## WebSocket support

Annotate a method with `@WebSocket("/path")` to create a WebSocket endpoint. The method must return `WsPipe[In, Out]` (a stream transformer).

Each side of the pipe is encoded on its own:

| Message type | Frames |
|--------------|--------|
| `String` | text |
| `Array[Byte]` | binary |
| any other type with a JSON codec (see [JSON](#json)) | text, carrying its JSON |

The server pings the client every 13 seconds; set the interval with `@WebSocket("/path", pingIntervalSeconds = 30)`,
or disable pinging with `pingIntervalSeconds = 0`.

Return types for `@WebSocket`:

| Return type | Meaning |
|-------------|---------|
| `WsPipe[In, Out]` | Pure — no setup effect |
| `ZIO[R, E, WsPipe[In, Out]]` (`E <: ApiError \| Throwable`) | Effectful — runs setup on connect, then streams; `R` joins the routes' environment |

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
- Any `ZIO[R, E, T]` with `E <: ApiError | Throwable`: `IO[ApiError, T]`, `IO[ApiError.NotFound, T]`, `UIO[T]`, `Task[T]`, `RIO[R, T]`, `URIO[R, T]`, `ZIO[R, ApiError | Throwable, T]`
- `FileResponse` — binary download; sets `Content-Disposition` from the filename (quotes/CR/LF stripped)
- `ZStream[Any, Throwable, Byte]` — chunked streaming, with the `@Produces` media type (`application/octet-stream` by default)
- `ZStream[Any, Throwable, ServerSentEvent]` (`sttp.model.sse.ServerSentEvent`) — a `text/event-stream` of server-sent events
- `ApiResponse[A]` — `A` with a status and headers chosen per call, see [Per-call responses](#per-call-responses)

Failures are mapped as follows:

| Failure | Response |
|---------|----------|
| an `ApiError` (see below) | its status, body `{"code": ..., "message": ...}` |
| `ApiErrorException(apiError)` — failed, or died with via `.orDie` | the wrapped `ApiError` |
| any other `Throwable`, or a defect (`ZIO.die`) | `500 Internal server error`; the cause is logged with `ZIO.logErrorCause`, its message is **not** sent |
| interruption | propagated |

**Environment.** The environment `R` of every method is collected into the routes' type:
`Okapi.httpRoutes[C]` is a `Routes[C & R1 & R2 ..., Response]`, so a missing layer is a compile error at
`.provide`. Requiring the controller itself (or a supertype of it) adds nothing.

**Scope.** A method needing a `Scope` gets a fresh scope per request, closed when the method's effect
completes; `Scope` never appears in the routes' type. Do not use a scoped resource from a `ZStream` or
WebSocket pipe returned by such a method — it is already released when the stream runs.

### Per-call responses

`ApiResponse[A]` (in `io.okapi.core.http`, effect-agnostic) carries a body together with a status and headers decided
at run time. `A` is documented as the body; without `withStatus` the endpoint's success status is sent.

```scala
@Get("/{id}/image")
def image(@Path("id") id: String): IO[ApiError, ApiResponse[Array[Byte]]] =
  images.load(id).map(img => ApiResponse(img.bytes).withContentType(img.mediaType))

@Get("/ready")
def ready: UIO[ApiResponse[Readiness]] =
  check.map(r => if r.ok then ApiResponse(r) else ApiResponse(r).withStatus(StatusCode.ServiceUnavailable))

@Post("/session")
def open: ApiResponse[Unit] = ApiResponse(()).withHeader("Mcp-Session-Id", newId())
```

A body differing per status can be a sealed hierarchy (see [JSON](#json)). `ApiResponse[FileResponse]` is not
supported: return `ApiResponse[Array[Byte]]` with a `Content-Disposition` header.

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
| `ApiError.ServiceUnavailable(msg)` | 503 |
| `ApiError.Other(status, msg)` | `status` (must be a 4xx or 5xx code) |

---

## Okapi API

```scala
// Generate ZIO HTTP routes for controllers
Okapi.routes[Controllers]                           // Routes[C1 & C2 & ... & R, Response]
Okapi.httpRoutes[MyController]                      // Routes[MyController & R, Response]
Okapi.routes[Controllers](options)                  // with Tapir server options (interceptors), see below

// Swagger UI (/docs) and the OpenAPI 3 document; extra: non-Okapi endpoints to include,
// customise: edit the document, e.g. OkapiDocs.withBearerAuth()
Okapi.swagger[Controllers]("Title", "1.0.0", extra = Nil, customise = identity)   // Routes[Any, Response]
Okapi.openApiYaml[Controllers]("Title", "1.0.0")    // String
Okapi.openApiJson[Controllers]("Title", "1.0.0")    // String

// Generate Tapir endpoint list (for custom handling)
Okapi.endpoints[MyController]                       // List[ZServerEndpoint[MyController & R, WebSockets]]

// Auto-wire all dependencies into a single ZLayer
Okapi.autoLayer[Controllers]                        // ZLayer[In, E, C1 & C2 & ...], see autoLayer below

// Manual layer registration (older API)
Okapi.registerOkapiControllers[Controllers](effect)
```

`Controllers` is a tuple type: `(BookController, UserController, AdminController)`. `R` is what the
controllers' methods need from the ZIO environment (see [Return types](#return-types-http-endpoints)).

---

## Server options, CORS and metrics

`Okapi.routes` / `Okapi.httpRoutes` take Tapir's `ZioHttpServerOptions[Any]`, so any Tapir interceptor applies to
the generated routes — CORS, metrics, logging, custom error handling:

```scala
import sttp.tapir.server.interceptor.cors.CORSInterceptor
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import io.okapi.metrics.OkapiPrometheus

val metrics = OkapiPrometheus[Task]()           // okapi-metrics, any effect
val options = ZioHttpServerOptions
  .customiseInterceptors[Any]
  .corsInterceptor(CORSInterceptor.default[Task])
  .metricsInterceptor(metrics.metricsInterceptor())
  .options

val app = Okapi.routes[Controllers](options) ++ ZioHttpInterpreter().toHttp(metrics.metricsEndpoint)  // GET /metrics
```

`OkapiPrometheus[F]` registers, per request:

| Metric | Labels |
|--------|--------|
| `okapi_request_active` (gauge) | `method`, `path`, `controller` |
| `okapi_request_total` (counter) | `method`, `path`, `controller`, `status` |
| `okapi_request_duration_seconds` (histogram) | `method`, `path`, `controller`, `status`, `phase` |

`path` is the route template (`/api/books/{id}`), `controller` the controller's tag (`@Tag` / `@ApiTag`, else its
class name), `status` the status class (`2xx`, ...). Pass `namespace` / `registry` to change the prefix or the
`PrometheusRegistry`.

For another metrics system, `OkapiMetrics.interceptor[F](record)` calls `record` once per request with a
`RequestRecord(method, path, controller, status, duration)` — `path` again the route template:

```scala
val options = ZioHttpServerOptions
  .customiseInterceptors[Any]
  .metricsInterceptor(OkapiMetrics.interceptor[Task](r => myMetrics.recordRequest(r.method, r.path, r.status, r.duration)))
  .options
```

zio-http middleware (`HandlerAspect`) applies to the generated routes as to any others —
`Okapi.routes[Controllers] @@ myMiddleware` — and its environment joins the routes' type.

---

## autoLayer

`Okapi.autoLayer[Controllers]` is a compile-time macro that:

1. Reads the primary constructor of each controller type
2. For each constructor parameter (service), recursively reads its constructor
3. Builds a topologically-sorted list of all required types (repos → services → controllers)
4. Generates `ZLayer.make[C1 & C2 & ...]` (or `ZLayer.makeSome`, see below) with `ZLayer.derive[T]` for every
   discovered type

The result is a `ZLayer[In, E, C1 & C2 & ...]`:
- `In` — dependencies that cannot be derived: traits, abstract classes and library types (`scala.*`, `java.*`,
  `javax.*`, `zio.*`, `sttp.*`, e.g. `String`, `DataSource`, `zio.http.Client`). `Any` when there are none. Supply
  them next to it: `.provide(Okapi.autoLayer[Controllers], Clock.live)`.
- `E` — what derivation can fail with: `Nothing` for plain constructors, e.g. `Config.Error` when a dependency
  is read from a `zio.Config` (a `ZLayer.Derive.Default`) or has a `ZLayer.Derive.Scoped` lifecycle.

Each type instantiation is its own dependency (`Store[User]` and `Store[Book]` get separate layers), and
`using`/`implicit` constructor parameters are left to `ZLayer.derive`.

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

## Parameters

- Parameter clauses: explicit clauses take request values; `using` / `implicit` clauses (e.g. ZIO's `Trace`) are
  resolved where the endpoints are generated. Curried methods and overloads are supported.
- **Default values** (`@Query("limit") limit: Int = 20`) make `@Query` / `@Header` / `@Cookie` parameters optional:
  when absent, the method's own default is used. `@Path` / `@BearerAuth` are always required.
- `@Path` parameters missing from the template are appended as trailing path captures.
- **No 22-parameter limit**: Tapir flattens at most 22 inputs into one tuple, so larger endpoints are transparently
  split into groups of inputs (up to 484 in total); the URL and the OpenAPI docs are unaffected.
- **Inheritance**: annotations are inherited from overridden methods (and their parameters) and from base classes,
  so an annotated API trait can be implemented by an unannotated class.

Routes are ordered most specific first (fixed segments before captures at each position), across REST and
WebSocket endpoints.

---

## Effect-agnostic core

`okapi-core` has no ZIO effect dependency. A controller written in tagless-final style works with any
effect that has an `OkapiEffect[F]`:

```scala
trait OkapiEffect[F[_]] {
  def monad: sttp.monad.MonadError[F]
  def pure[A](value: => A): F[A]
  def fail[A](error: ApiError): F[A]
  def attempt[A](fa: F[A]): F[Either[ApiError, A]]
}
```

- `OkapiEffect.fromMonadError[F]` covers effects whose error channel is `Throwable` (cats-effect `IO`,
  `Future`, `Try`, ...): an `ApiError` travels through `F` as `ApiErrorException`.
- `OkapiEndpoints[F].of(controller)` returns `List[ServerEndpoint[Any, F]]` for a controller *instance*;
  serve it with any Tapir interpreter for `F`.
- Method types are read as seen from the controller, so `class Api[F[_]]` used as `Api[IO]` yields `IO[A]`.
- `ZStream` bodies and `@WebSocket` endpoints are ZIO-specific and need `okapi-zio`.

Under the hood a `ControllerHost[C, F, G]` decides how the generated server logic obtains controller `C`
and runs its `F` effects in the server effect `G` — a fixed instance in the core, the ZIO environment
(`G = RIO[C & R, *]`) in `okapi-zio`.

---

## Clients

Two ways to call an Okapi API from Scala.

### From the API trait (`okapi-client`)

Describe the API once as an annotated trait; the server implements it, the client is made from it at compile time
— same types, same routes, nothing generated on disk:

```scala
@Controller("/api/books")
trait BookApi[F[_]] {
  @Get("/{id}") def get(@Path("id") id: Int): F[Book]
  @Post("") def create(@RequestBody book: Book): F[Book]
}

final class BookServer(using F: OkapiEffect[IO]) extends BookApi[IO] { ... }   // served with OkapiEndpoints / Okapi

val books: BookApi[IO] = OkapiClient[IO].of[BookApi[IO]](uri"http://books:8080", backend)  // sttp client4 Backend[IO]
books.get(1)                                                                              // IO[Book]
```

Every abstract method must be routed and return `F[A]`. An error response fails `F` with the `ApiError` for its status
(`ApiErrorException` for `Throwable`-based effects); `ApiResponse` and `FileResponse` results work as on the server.
Put the trait (and its models) in a small module both the server and its clients depend on.

### Generated files (`sbt-okapi`)

When a client must exist as source files — published as `<api>-client`, or reviewed — the sbt plugin generates
them from the API's OpenAPI document:

```scala
// project/plugins.sbt
addSbtPlugin("io.github.andrzej-swietek" % "sbt-okapi" % "0.2.0")

// build.sbt
lazy val api = project
  .enablePlugins(OkapiPlugin)
  .settings(okapiSpec := "com.example.ApiSpec.yaml")   // object ApiSpec { def yaml = Okapi.openApiYaml[Controllers]("API", "1") }

lazy val apiClient = project.in(file("api-client"))    // the generated module
```

| Task / setting | |
|----------------|--|
| `okapiSpecFile` | writes the document to `target/okapi/openapi.yaml` (runs `okapiSpec` on the test classpath) |
| `okapiGenerateClient` | writes Tapir endpoints, models with jsoniter-scala codecs, and a `build.sbt` (if absent) to `okapiClientDirectory` |
| `okapiClientName` | `<name>-client` |
| `okapiClientPackage` / `okapiClientObject` | `<name>.client` / `<Name>Endpoints` |
| `okapiClientDirectory` | `<base>/<okapiClientName>` |
| `okapiClientStreaming` | streams for binary bodies: `fs2` (default) or `zio` |

Call a generated endpoint with Tapir's sttp client interpreter:
`SttpClientInterpreter().toClientThrowDecodeFailures(ApiEndpoints.getApiBooksId, Some(uri), backend)(id)`; error
responses come back as `Left(ApiErrorResponse(...))`. The same `openapi.yaml` feeds other generators for clients in
other languages. Run `okapiGenerateClient` and the client module's compilation in separate sbt sessions, since sbt
reads the generated `build.sbt` on start.

---

## Upgrading from 0.1.x

- Depend on `okapi-zio` (it brings `okapi-core`); package names and the `Okapi` API are unchanged.
- zio-json `derives JsonCodec` keeps working with okapi-zio; `okapi-core` alone no longer depends on zio-json, and
  `ApiErrorResponse` no longer has a zio-json `JsonCodec` given.
- A case class body with a media type other than JSON or a form (e.g. `@Produces("text/plain") def x: Book`) is
  now a compile error; in 0.1.x it was silently encoded as JSON or failed to compile with a type mismatch.

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

- **`multipart/form-data` with `-Yexplicit-nulls`** — Tapir's derived `MultipartCodec` (from `sttp.tapir.generic.auto.*`)
  expands in the file generating the endpoints and does not compile there under `-Yexplicit-nulls`. Add
  `import scala.language.unsafeNulls` to that file. A form is a case class of fields, with `sttp.model.Part[Array[Byte]]`
  for a file (`part.fileName`, `part.body`).

- **Several documented responses (`oneOf`)** — `ApiResponse` picks the status per call, but the OpenAPI document
  shows one body type per endpoint; use a sealed hierarchy to describe different shapes.

- **Security schemes beyond bearer** — `@BearerAuth` covers bearer tokens (passed to the controller and advertised in OpenAPI). API-key / OAuth2 `securityIn` schemes and the `serverSecurityLogic` split are not generated; use `@Header` for those for now.

- **Request validation** — no `@Min`, `@Max`, `@NotBlank` etc. Input validation must be done in the controller/service body.

- **Paths from runtime configuration** — routes are fixed at compile time by the annotations.

### Known gotchas

- `routes[Controllers]` and `swagger[Controllers]` must be `lazy val` (not `val`) in `object Main` to avoid JVM `Method too large` error when there are many endpoints.

- `@Tag` from `io.okapi.core.annotations` collides with `zio.Tag` under `import zio.*`. Either use `import zio.{ IO, ZIO }`, or use the **`@ApiTag`** alias, which never collides.

- `java.lang.System.currentTimeMillis()` — `import zio.*` shadows `System` with `zio.System`. Use the fully qualified name.

- After changing any module, run `sbt publishLocal` from the project root before compiling `okapi-example/`.

- Annotation arguments must be known at compile time: literals (`@Get("/x")`, `@Get(path = "/x")`) or `final val` constants; anything else is a compile error.
