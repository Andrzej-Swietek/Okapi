# Okapi

[![Maven Central](https://img.shields.io/maven-central/v/io.github.andrzej-swietek/okapi-zio_3?label=Maven%20Central)](https://central.sonatype.com/artifact/io.github.andrzej-swietek/okapi-zio_3)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue)](LICENSE)

**Annotation-driven HTTP APIs for Scala 3 — [ZIO HTTP](https://zio.dev/zio-http/) + [Tapir](https://tapir.softwaremill.com/), with zero boilerplate.**

Okapi is a Scala 3 macro library that turns annotated controller classes into fully-wired Tapir
endpoints served by ZIO HTTP, with Swagger UI, and derives the entire `ZLayer` dependency graph.
The endpoints and the layer are generated at **compile time**, without runtime reflection.

The core is effect-agnostic (tagless final): ZIO is one backend, and any effect `F[_]` can be
plugged in through the `OkapiEffect[F]` type class.

## Installation

```scala
// build.sbt
libraryDependencies += "io.github.andrzej-swietek" %% "okapi-zio" % "0.2.0"

// required — the macros expand deeply
scalacOptions += "-Xmax-inlines:128"
```

| Module | Contents |
|--------|----------|
| `okapi-zio` | ZIO HTTP routes, `ZStream` / SSE / WebSocket bodies, `ZLayer` wiring (brings `okapi-core`, `okapi-openapi`) |
| `okapi-core` | effect-agnostic annotations, macros and runtime — no ZIO dependency |
| `okapi-openapi` | OpenAPI JSON/YAML and Swagger UI, any effect |
| `okapi-metrics` | endpoint metrics (per-request callback, Prometheus), any effect |
| `okapi-client` | HTTP clients generated from annotated API traits, any effect |
| `okapi-codegen` | tagless-final sttp client modules generated from an OpenAPI document |
| `sbt-okapi` | sbt plugin generating a `<name>-client` module from the API's OpenAPI document, with `okapi-codegen` |

Requires **Scala 3.6+**. Published for Scala 3 on Maven Central — no extra resolvers, no authentication.

For Gradle / Maven, the coordinates are `io.github.andrzej-swietek:okapi-zio_3:0.2.0`.
Upgrading from 0.1.x: replace the `okapi-core` dependency with `okapi-zio` — see [Upgrading](OKAPI.md#upgrading-from-01x).

## Quick start

```scala
import io.okapi.core.Okapi
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

import io.okapi.core.json.JsoniterCodec
import zio.{ IO, ZIO, ZLayer, ZIOAppDefault }
import zio.http.{ Server, Routes, Response }

final case class Book(id: Int, title: String) derives JsoniterCodec

final class BookService {
  def get(id: Int): IO[ApiError, Book] =
    ZIO.succeed(Book(id, s"Book #$id"))
}

@Controller("/api/books")
@Tag("Books")
final class BookController(service: BookService) {

  @Get("/{id}")
  @Summary("Get a book by id")
  def getBook(@Path("id") id: Int): IO[ApiError, Book] =
    service.get(id)
}

object Main extends ZIOAppDefault {
  private type Controllers = Tuple1[BookController] // for many: (BookController, UserController, ...)

  // keep these `lazy val` to avoid a JVM "Method too large" error with many endpoints
  private lazy val app: Routes[BookController, Response] =
    Okapi.routes[Controllers] ++ Okapi.swagger[Controllers]("Books API", "1.0.0")

  override def run =
    Server
      .serve(app)
      .provide(
        ZLayer.succeed(Server.Config.default.port(8080)),
        Server.live,
        Okapi.autoLayer[Controllers], // discovers BookService (and its deps) automatically
      )
}
```

Run it and open `http://localhost:8080/docs` for the Swagger UI.

Methods may return any `ZIO[R, ApiError | Throwable, A]` (`IO`, `UIO`, `Task`, `URIO[Repo, A]`, ...) or a
plain `A` — see [Return types](OKAPI.md#return-types-http-endpoints).

JSON is pluggable: jsoniter-scala by default (as above), any Tapir JSON integration by import, and zio-json with
okapi-zio — see [JSON](OKAPI.md#json).

### Without ZIO

```scala
import io.okapi.core.{ OkapiEffect, OkapiEndpoints }
import sttp.tapir.server.ServerEndpoint

@Controller("/api/books")
final class BookController[F[_]](using F: OkapiEffect[F]) {
  @Get("/{id}")
  def getBook(@Path("id") id: Int): F[Book] =
    if id > 0 then F.pure(Book(id, "Dune")) else F.fail(ApiError.NotFound(s"book $id"))
}

// with an sttp.monad.MonadError[IO] in scope (tapir's cats-effect integration provides one)
given OkapiEffect[IO] = OkapiEffect.fromMonadError[IO]
val endpoints: List[ServerEndpoint[Any, IO]] = OkapiEndpoints[IO].of(BookController[IO]())
// serve with any Tapir interpreter for IO (http4s, Netty, ...)
```

## What you get

- `@Controller` / `@Get` / `@Post` / `@Put` / `@Delete` / `@Patch` / `@WebSocket` routing from annotations
- `@Path` / `@Query` / `@Header` / `@Cookie` / `@RequestBody` / `@BearerAuth` parameter binding
- `@Consumes` / `@Produces` content types (JSON, form, multipart, any text or binary media type, SSE, …)
- pluggable JSON (jsoniter-scala, zio-json, circe, … — any Tapir integration)
- no 22-parameter limit, default parameter values, `using` clauses, annotated API traits
- success status codes inferred (`204` for a `Unit` result, `201` for `@Post`, else `200`) or set with `@Status(code)`
- `ApiError` → HTTP status mapping; any other failure → a logged `500` without the exception message
- per-call status, headers and `Content-Type` (`ApiResponse`), `FileResponse` downloads
- `ZStream` bodies, server-sent events, WebSocket pipes with text, binary or JSON frames
- Tapir server options on the generated routes (CORS, …), zio-http middleware, and metrics per route template
- `Okapi.autoLayer[Controllers]` — compile-time `ZLayer` wiring of the whole dependency tree
- Swagger UI + `Okapi.openApiYaml` / `Okapi.openApiJson`, with extra endpoints and document customisation
- clients: `OkapiClient[F].of[Api]` from an API trait, or a generated tagless-final `<name>-client` module
  (`sbt okapiGenerateClient`): a trait per controller, models with jsoniter-scala codecs, an sttp implementation, fs2
  or ZIO streams and server-sent events

## Repository layout

| Directory | |
|-----------|--|
| `core/` | `okapi-core` |
| `modules/` | `zio/`, `openapi/`, `metrics/` — `okapi-zio`, `okapi-openapi`, `okapi-metrics` |
| `client/` | `derived/` (`okapi-client`), `codegen/` (`okapi-codegen`), `codegen-it/` (compiles and runs a generated client) |
| `sbt/` | `sbt-okapi` |
| `example/` | `server/` (an Okapi API) and `client/` (its generated client) — separate sbt builds |

## Documentation

The complete reference — every annotation, all supported media types, WebSockets, `autoLayer`,
error handling, and current limitations — is in **[OKAPI.md](OKAPI.md)**.

## License

[Apache-2.0](LICENSE)
