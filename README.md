# Okapi

[![Maven Central](https://img.shields.io/maven-central/v/io.github.andrzej-swietek/okapi-core_3?label=Maven%20Central)](https://central.sonatype.com/artifact/io.github.andrzej-swietek/okapi-core_3)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue)](LICENSE)

**Annotation-driven HTTP APIs for Scala 3 — [ZIO HTTP](https://zio.dev/zio-http/) + [Tapir](https://tapir.softwaremill.com/), with zero boilerplate.**

Okapi is a Scala 3 macro library that turns annotated controller classes into fully-wired Tapir
endpoints served by ZIO HTTP, generates Swagger UI, and auto-derives the entire `ZLayer`
dependency graph — all at **compile time**, with no runtime reflection.

## Installation

```scala
// build.sbt
libraryDependencies += "io.github.andrzej-swietek" %% "okapi-core" % "0.1.3"

// required — the macros expand deeply
scalacOptions += "-Xmax-inlines:128"
```

Requires **Scala 3.6+**. Published for Scala 3 on Maven Central — no extra resolvers, no authentication.

For Gradle / Maven, the coordinates are `io.github.andrzej-swietek:okapi-core_3:0.1.3`.

## Quick start

```scala
import io.okapi.core.Okapi
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

import sttp.tapir.generic.auto.*
import zio.{ IO, ZIO, ZLayer, ZIOAppDefault }
import zio.http.{ Server, Routes, Response }
import zio.json.JsonCodec

final case class Book(id: Int, title: String) derives JsonCodec

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
  private lazy val app: Routes[BookService, Response] =
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

## What you get

- `@Controller` / `@Get` / `@Post` / `@Put` / `@Delete` / `@Patch` / `@WebSocket` routing from annotations
- `@Path` / `@Query` / `@Header` / `@Cookie` / `@RequestBody` parameter binding
- `@Consumes` / `@Produces` content negotiation (JSON, form, multipart, text, XML, binary, SSE, …)
- `ApiError` → HTTP status mapping, `FileResponse` downloads, and WebSocket pipes
- `Okapi.autoLayer[Controllers]` — compile-time `ZLayer` wiring of the whole dependency tree
- Swagger UI generation

## Documentation

The complete reference — every annotation, all supported media types, WebSockets, `autoLayer`,
error handling, and current limitations — is in **[OKAPI.md](OKAPI.md)**.

## License

[Apache-2.0](LICENSE)
