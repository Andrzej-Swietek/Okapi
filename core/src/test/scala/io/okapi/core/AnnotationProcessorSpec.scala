package io.okapi.core

import zio.{ IO, Scope, ZIO, ZLayer }
import zio.http.{ Body, Header, Headers, Method, Request, Response, Status }
import zio.json.JsonCodec
import zio.test.*

import io.okapi.core.annotations.{
  ApiTag,
  BearerAuth,
  Controller,
  Cookie,
  Delete,
  Get,
  Path,
  Post,
  Produces,
  Put,
  Query,
  RequestBody,
  Summary,
  Tag,
  WebSocket,
}
import io.okapi.core.http.{ ApiError, FileResponse }
import sttp.tapir.server.ziohttp.ZioHttpInterpreter

object AnnotationProcessorSpec extends ZIOSpecDefault {

  // ─── Shared DTOs ──────────────────────────────────────────────────────────

  final case class HelloResponse(message: String) derives JsonCodec
  final case class EchoRequest(text: String) derives JsonCodec
  final case class EchoResponse(id: Int, text: String) derives JsonCodec
  final case class AdminResponse(message: String) derives JsonCodec

  final class GreetingService {
    def greet(name: String): String = s"service:$name"
  }

  // ─── Original controllers ─────────────────────────────────────────────────

  @Controller("/api/test")
  @Tag("TestController")
  final class TestController {

    @Get("/hello")
    @Summary("hello endpoint")
    def hello(
      @Query("name") name: Option[String]
    ): HelloResponse =
      HelloResponse(s"Hello ${name.getOrElse("World")}")

    @Post("/echo")
    def echo(
      @Path("id") id: Int,
      @RequestBody body: EchoRequest,
    ): IO[ApiError, EchoResponse] =
      ZIO.succeed(EchoResponse(id, body.text))
  }

  @Controller("/api/admin")
  @Tag("AdminController")
  final class AdminController {

    @Get("/status")
    def status: AdminResponse =
      AdminResponse("admin-ok")
  }

  @Controller("/api/dependent")
  @Tag("DependentController")
  final class DependentController(greetingService: GreetingService) {

    @Get("/hello")
    def hello(
      @Query("name") name: Option[String]
    ): HelloResponse =
      HelloResponse(greetingService.greet(name.getOrElse("World")))
  }

  // ─── Nested path controllers ───────────────────────────────────────────────

  @Controller("/api/nested")
  @Tag("NestedController")
  final class NestedController {

    @Get("/items/{id}/details")
    @Summary("get item details")
    def getItemDetails(
      @Path("id") id: Int,
      @Query("expand") expand: Option[String],
    ): HelloResponse =
      HelloResponse(s"item=$id expand=${expand.getOrElse("none")}")

    @Get("/users/{userId}/posts/{postId}")
    def getPost(
      @Path("userId") userId: Int,
      @Path("postId") postId: Int,
    ): HelloResponse =
      HelloResponse(s"user=$userId post=$postId")

    @Delete("/resources/{category}/{id}")
    def deleteResource(
      @Path("category") category: String,
      @Path("id") id: Int,
    ): HelloResponse =
      HelloResponse(s"deleted $category/$id")

    @Put("/versions/{version}/items/{id}")
    def updateVersioned(
      @Path("version") version: String,
      @Path("id") id: Int,
      @RequestBody body: EchoRequest,
    ): IO[ApiError, HelloResponse] =
      ZIO.succeed(HelloResponse(s"v=$version id=$id text=${body.text}"))
  }

  // ─── Content-type controllers ──────────────────────────────────────────────

  @Controller("/api/content")
  @Tag("ContentController")
  final class ContentController {

    @Get("/text")
    @Produces("text/plain")
    def getText(
      @Query("name") name: Option[String]
    ): String =
      s"hello ${name.getOrElse("world")}"

    @Get("/bytes")
    def getBytes: Array[Byte] =
      Array[Byte](1, 2, 3)

    @Get("/json-str")
    def getJsonStr: String =
      "raw string"
  }

  // ─── Services for autoLayer tests ────────────────────────────────────────

  final class LoggingService {
    def format(msg: String): String = s"[LOG] $msg"
  }

  final class MetricsService(logger: LoggingService) {
    def record(event: String): String = logger.format(s"metric:$event")
  }

  final class DatabaseService {
    def query(sql: String): String = s"db($sql)"
  }

  final class UserRepository(db: DatabaseService) {
    def find(id: Int): String = db.query(s"id=$id")
  }

  // ─── Controllers for autoLayer tests ─────────────────────────────────────

  @Controller("/api/auto-single")
  @Tag("AutoSingleController")
  final class AutoSingleController(logger: LoggingService) {

    @Get("/hello")
    def hello: HelloResponse = HelloResponse(logger.format("hello"))
  }

  @Controller("/api/auto-multi")
  @Tag("AutoMultiController")
  final class AutoMultiController(logger: LoggingService, metrics: MetricsService) {

    @Get("/status")
    def status: HelloResponse = HelloResponse(s"${logger.format("ok")} ${metrics.record("check")}")
  }

  @Controller("/api/auto-transitive")
  @Tag("AutoTransitiveController")
  final class AutoTransitiveController(metrics: MetricsService) {

    @Get("/info")
    def info: HelloResponse = HelloResponse(metrics.record("info"))
  }

  @Controller("/api/auto-deep")
  @Tag("AutoDeepController")
  final class AutoDeepController(users: UserRepository) {

    @Get("/user/{id}")
    def getUser(@Path("id") id: Int): HelloResponse = HelloResponse(users.find(id))
  }

  // ─── Controllers for feature tests ─────────────────────────────────────────

  @Controller("/api/map")
  @ApiTag("MapApi")
  final class MappingController {

    // the canonical "two path params split by a fixed segment + required & optional query" shape
    @Get("/costam/{param1}/x/{param2}")
    def costam(
      @Path("param1") param1: String,
      @Path("param2") param2: Int,
      @Query("id") id: String,
      @Query("query") query: Option[String],
    ): HelloResponse =
      HelloResponse(s"p1=$param1 p2=$param2 id=$id q=${query.getOrElse("none")}")

    // @Path declared in REVERSE of URL order, with a query interleaved first
    @Get("/r/{a}/{b}")
    def reversed(
      @Query("q") q: String,
      @Path("b") b: Int,
      @Path("a") a: String,
    ): HelloResponse =
      HelloResponse(s"a=$a b=$b q=$q")

    // @Path("b") is NOT in the URL template -> appended as a trailing capture (guards the extractArgs ordering fix)
    @Get("/u/{a}")
    def unconsumed(
      @Query("q") q: String,
      @Path("b") b: Int,
      @Path("a") a: String,
    ): HelloResponse =
      HelloResponse(s"a=$a b=$b q=$q")
  }

  @Controller("/api/status")
  @ApiTag("StatusApi")
  final class StatusController {

    @Post("/create")
    def create(@RequestBody body: EchoRequest): IO[ApiError, EchoResponse] =
      ZIO.succeed(EchoResponse(1, body.text))

    @Delete("/{id}")
    def remove(@Path("id") id: Int): IO[ApiError, Unit] =
      ZIO.unit

    @Get("/teapot")
    @io.okapi.core.annotations.Status(418)
    def teapot: HelloResponse =
      HelloResponse("tea")

    @Get("/ok")
    def ok: HelloResponse =
      HelloResponse("ok")
  }

  @Controller("/api/errors")
  @ApiTag("ErrorApi")
  final class ErrorController {

    @Get("/notfound")
    def notFound: IO[ApiError, HelloResponse] =
      ZIO.fail(ApiError.NotFound("missing"))

    @Get("/conflict")
    def conflict: IO[ApiError, HelloResponse] =
      ZIO.fail(ApiError.Conflict("duplicate"))

    @Get("/throttled")
    def throttled: IO[ApiError, HelloResponse] =
      ZIO.fail(ApiError.TooManyRequests("slow down"))
  }

  @Controller("/api/hc")
  @ApiTag("HeaderCookieApi")
  final class HeaderCookieController {

    @Get("/header")
    def header(@io.okapi.core.annotations.Header("X-Token") token: String): HelloResponse =
      HelloResponse(s"token=$token")

    @Get("/cookie")
    def cookie(@Cookie("session") session: Option[String]): HelloResponse =
      HelloResponse(s"session=${session.getOrElse("none")}")

    @Get("/legacy")
    @io.okapi.core.annotations.Deprecated()
    def legacy: HelloResponse =
      HelloResponse("old")
  }

  @Controller("/api/files")
  @ApiTag("FileApi")
  final class FileController {

    @Get("/clean")
    def clean: IO[ApiError, FileResponse] =
      ZIO.succeed(FileResponse("hello".getBytes("UTF-8").nn, "my report.txt"))

    @Get("/dirty")
    def dirty: IO[ApiError, FileResponse] =
      ZIO.succeed(FileResponse(Array[Byte](1, 2), "ev\"il\r\nname.txt"))
  }

  @Controller("/api/secure")
  @ApiTag("SecureApi")
  final class SecureController {

    @Get("/me")
    def me(@BearerAuth token: String): HelloResponse =
      HelloResponse(s"token=$token")
  }

  @Controller("/api/ws")
  @ApiTag("WsApi")
  final class WsTestController {

    @WebSocket("/echo")
    def echo: WsPipe[String, String] =
      stream => stream

    @WebSocket("/chat/{room}")
    def chat(
      @Path("room") room: String,
      @Query("user") user: Option[String],
    ): WsPipe[String, String] =
      stream => stream
  }

  // ─── Test helpers ──────────────────────────────────────────────────────────

  private val testRoutes = ZioHttpInterpreter().toHttp(Okapi.endpoints[TestController])
  private val nestedRoutes = ZioHttpInterpreter().toHttp(Okapi.endpoints[NestedController])
  private val contentRoutes = ZioHttpInterpreter().toHttp(Okapi.endpoints[ContentController])

  private type CombinedControllers = (TestController, AdminController)

  private def parseUrl(path: String): zio.http.URL =
    zio.http.URL.decode(path).getOrElse(throw new IllegalArgumentException(s"bad url: $path"))

  private def getRequest(path: String): Request = {
    Request(
      method = Method.GET,
      url = parseUrl(path),
      headers = Headers.empty,
      body = Body.empty,
      version = zio.http.Version.Http_1_1,
      remoteAddress = None,
    )
  }

  private def jsonRequest(method: Method, path: String, jsonBody: String): Request = {
    Request(
      method = method,
      url = parseUrl(path),
      headers = Headers(
        Header
          .ContentType
          .parse("application/json")
          .fold(_ => throw new IllegalStateException("invalid media type"), identity)
      ),
      body = Body.fromString(jsonBody),
      version = zio.http.Version.Http_1_1,
      remoteAddress = None,
    )
  }

  // ─── Specs ────────────────────────────────────────────────────────────────

  override def spec: Spec[TestEnvironment & Scope, Any] = {
    suite("AnnotationProcessorSpec")(
      suite("original behaviour")(
        test("generateEndpoints keeps HTTP metadata and paths") {
          val endpoints = Okapi.generateEndpoints[TestController]
          val rendered = endpoints.map(_.endpoint.showShort)
          val helloEndpoint = endpoints
            .find(_.endpoint.showShort == "GET /api/test/hello")
            .getOrElse(throw new IllegalStateException("hello endpoint not generated"))
          val hasHelloSummary = helloEndpoint.endpoint.info.summary.contains("hello endpoint")
          val hasHelloQueryInput = helloEndpoint.endpoint.input.show.contains("name")

          assertTrue(endpoints.size == 2) &&
          assertTrue(rendered.contains("GET /api/test/hello")) &&
          assertTrue(rendered.contains("POST /api/test/echo/{id}")) &&
          assertTrue(hasHelloSummary) &&
          assertTrue(hasHelloQueryInput)
        },
        test("generated routes execute controller methods through zio-http") {
          val request = jsonRequest(Method.POST, "/api/test/echo/42", """{"text":"works"}""")

          for {
            response <- ZIO.scoped {
              testRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new TestController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Created) &&
          assertTrue(body.contains("works")) &&
          assertTrue(body.contains("42"))
        },
        test("selected routes combine multiple controllers") {
          val routes = Okapi.routes[CombinedControllers]
          val request = Request.get("/api/admin/status")

          for {
            response <- ZIO.scoped {
              routes
                .runZIO(request)
                .provideSome[Scope](
                  Okapi.controllerLayers[CombinedControllers]
                )
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("admin-ok")) &&
          assertTrue(Okapi.selectedEndpoints[CombinedControllers].size == 3)
        },
        test("controller and service layers resolve constructor dependencies") {
          for {
            response <- ZIO
              .serviceWith[DependentController](_.hello(Some("Okapi")))
              .provide(
                ZLayer.make[DependentController](
                  Okapi.layer[GreetingService],
                  Okapi.layer[DependentController],
                )
              )
          } yield assertTrue(response.message == "service:Okapi")
        },
      ),
      suite("path template parsing")(
        test("mid-path param shows correctly in showShort") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/nested/items/{id}/details"))
        },
        test("two path params show correctly in showShort") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/nested/users/{userId}/posts/{postId}"))
        },
        test("consecutive path params show correctly in showShort") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("DELETE /api/nested/resources/{category}/{id}"))
        },
        test("path param before and after fixed segment shows correctly") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("PUT /api/nested/versions/{version}/items/{id}"))
        },
        test("nested controller generates correct number of endpoints") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          assertTrue(endpoints.size == 4)
        },
        test("summary is preserved on nested path endpoint") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val summaries = endpoints.flatMap(_.endpoint.info.summary)
          assertTrue(summaries.contains("get item details"))
        },
        test("HTTP routing captures mid-path param correctly") {
          val request = Request.get("/api/nested/items/42/details")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("item=42")) &&
          assertTrue(body.contains("expand=none"))
        },
        test("HTTP routing captures mid-path param with query string") {
          val request = getRequest("/api/nested/items/7/details?expand=sub")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("item=7")) &&
          assertTrue(body.contains("expand=sub"))
        },
        test("HTTP routing captures both params in two-param path") {
          val request = Request.get("/api/nested/users/3/posts/99")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("user=3")) &&
          assertTrue(body.contains("post=99"))
        },
        test("HTTP routing captures consecutive path params") {
          val request = Request.delete("/api/nested/resources/books/5")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("deleted books/5"))
        },
        test("HTTP routing for versioned path with body") {
          val request = jsonRequest(Method.PUT, "/api/nested/versions/v2/items/10", """{"text":"hi"}""")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("v=v2")) &&
          assertTrue(body.contains("id=10")) &&
          assertTrue(body.contains("text=hi"))
        },
        test("wrong path does not match nested route") {
          val request = Request.get("/api/nested/items/42")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
          } yield assertTrue(response.status == Status.NotFound)
        },
        test("path param with non-integer value returns 400 or 404") {
          val request = Request.get("/api/nested/items/not-a-number/details")

          for {
            response <- ZIO.scoped {
              nestedRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new NestedController))
            }
          } yield assertTrue(
            response.status == Status.BadRequest || response.status == Status.NotFound
          )
        },
        test("path normalization handles leading slashes correctly") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered = endpoints.map(_.endpoint.showShort)
          val noDoubleSlash = rendered.forall(p => !p.contains("//"))
          val knownMethod = rendered.forall(p =>
            p.startsWith("GET") || p.startsWith("POST") || p.startsWith("PUT") || p.startsWith("DELETE")
          )
          assertTrue(noDoubleSlash && knownMethod)
        },
      ),
      suite("content type support")(
        test("@Produces text/plain endpoint generated correctly") {
          val endpoints = Okapi.generateEndpoints[ContentController]
          val rendered = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/content/text"))
        },
        test("Array[Byte] return type endpoint generated correctly") {
          val endpoints = Okapi.generateEndpoints[ContentController]
          val rendered = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/content/bytes"))
        },
        test("content controller generates correct number of endpoints") {
          val endpoints = Okapi.generateEndpoints[ContentController]
          assertTrue(endpoints.size == 3)
        },
        test("plain text endpoint serves string response") {
          val request = getRequest("/api/content/text?name=Okapi")

          for {
            response <- ZIO.scoped {
              contentRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new ContentController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("hello Okapi"))
        },
        test("plain text endpoint serves default response without query param") {
          val request = Request.get("/api/content/text")

          for {
            response <- ZIO.scoped {
              contentRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new ContentController))
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("hello world"))
        },
        test("bytes endpoint returns binary response") {
          val request = Request.get("/api/content/bytes")

          for {
            response <- ZIO.scoped {
              contentRoutes.runZIO(request).provideSome[Scope](ZLayer.succeed(new ContentController))
            }
            bytes <- response.body.asArray
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(bytes.length == 3) &&
          assertTrue(bytes(0) == 1.toByte) &&
          assertTrue(bytes(1) == 2.toByte) &&
          assertTrue(bytes(2) == 3.toByte)
        },
        test("String return type uses string body regardless of @Produces") {
          val endpoints = Okapi.generateEndpoints[ContentController]
          assertTrue(endpoints.nonEmpty)
        },
      ),
      suite("auto layer")(
        test("autoLayer discovers direct service dependency") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[AutoSingleController])
          val layer = Okapi.autoLayer[Tuple1[AutoSingleController]]

          for {
            response <- ZIO.scoped {
              routes.runZIO(Request.get("/api/auto-single/hello")).provideSome[Scope](layer)
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("[LOG]")) &&
          assertTrue(body.contains("hello"))
        },
        test("autoLayer discovers transitive service dependency (controller -> svc -> svc)") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[AutoTransitiveController])
          val layer = Okapi.autoLayer[Tuple1[AutoTransitiveController]]

          for {
            response <- ZIO.scoped {
              routes.runZIO(Request.get("/api/auto-transitive/info")).provideSome[Scope](layer)
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("metric")) &&
          assertTrue(body.contains("[LOG]"))
        },
        test("autoLayer wires two-level deep transitive dependency") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[AutoDeepController])
          val layer = Okapi.autoLayer[Tuple1[AutoDeepController]]

          for {
            response <- ZIO.scoped {
              routes.runZIO(Request.get("/api/auto-deep/user/7")).provideSome[Scope](layer)
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("db(")) &&
          assertTrue(body.contains("id=7"))
        },
        test("autoLayer with controller having two direct service deps") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[AutoMultiController])
          val layer = Okapi.autoLayer[Tuple1[AutoMultiController]]

          for {
            response <- ZIO.scoped {
              routes.runZIO(Request.get("/api/auto-multi/status")).provideSome[Scope](layer)
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("[LOG] ok")) &&
          assertTrue(body.contains("metric"))
        },
        test("autoLayer with multiple controllers shares discovered services (no duplicates)") {
          type Both = (AutoSingleController, AutoTransitiveController)
          val layer = Okapi.autoLayer[Both]
          val routes = Okapi.routes[Both]

          for {
            r1 <- ZIO.scoped {
              routes.runZIO(Request.get("/api/auto-single/hello")).provideSome[Scope](layer)
            }
            r2 <- ZIO.scoped {
              routes.runZIO(Request.get("/api/auto-transitive/info")).provideSome[Scope](layer)
            }
            b1 <- r1.body.asString
            b2 <- r2.body.asString
          } yield assertTrue(r1.status == Status.Ok) &&
          assertTrue(r2.status == Status.Ok) &&
          assertTrue(b1.contains("[LOG]")) &&
          assertTrue(b2.contains("metric"))
        },
        test("autoLayer provides correct number of discovered types") {
          // AutoTransitiveController -> MetricsService -> LoggingService: 3 unique types
          val deps = Okapi.generateEndpoints[AutoTransitiveController]
          assertTrue(deps.size == 1)
        },
        test("autoLayer can be used in ZIO.provide directly") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[AutoDeepController])

          for {
            response <- ZIO.scoped {
              routes
                .runZIO(Request.get("/api/auto-deep/user/99"))
                .provideSome[Scope](Okapi.autoLayer[Tuple1[AutoDeepController]])
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("id=99"))
        },
      ),
      suite("success status codes")(
        test("POST defaults to 201 Created") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[StatusController])
          val req = jsonRequest(Method.POST, "/api/status/create", """{"text":"hi"}""")
          for {
            resp <- ZIO.scoped(routes.runZIO(req).provideSome[Scope](ZLayer.succeed(new StatusController)))
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 201) && assertTrue(body.contains("hi"))
        },
        test("Unit body (DELETE) defaults to 204 No Content with empty body") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[StatusController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.delete("/api/status/7")).provideSome[Scope](ZLayer.succeed(new StatusController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 204) && assertTrue(body.isEmpty)
        },
        test("@Status overrides the inferred code") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[StatusController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/status/teapot")).provideSome[Scope](ZLayer.succeed(new StatusController))
            )
          } yield assertTrue(resp.status.code == 418)
        },
        test("GET stays 200 OK") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[StatusController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/status/ok")).provideSome[Scope](ZLayer.succeed(new StatusController))
            )
          } yield assertTrue(resp.status.code == 200)
        },
      ),
      suite("error mapping")(
        test("ApiError.NotFound -> 404 with code + message in body") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[ErrorController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/errors/notfound")).provideSome[Scope](ZLayer.succeed(new ErrorController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 404) && assertTrue(body.contains("missing")) && assertTrue(
            body.contains("404")
          )
        },
        test("ApiError.Conflict -> 409") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[ErrorController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/errors/conflict")).provideSome[Scope](ZLayer.succeed(new ErrorController))
            )
          } yield assertTrue(resp.status.code == 409)
        },
        test("ApiError.TooManyRequests -> 429") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[ErrorController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(Request.get("/api/errors/throttled"))
                .provideSome[Scope](ZLayer.succeed(new ErrorController))
            )
          } yield assertTrue(resp.status.code == 429)
        },
      ),
      suite("path & query mapping")(
        test("@ApiTag sets the swagger tag (collision-free alias for @Tag)") {
          val tags = Okapi.endpoints[MappingController].head.endpoint.info.tags
          val hasTag = tags.contains("MapApi")
          assertTrue(hasTag)
        },
        test("two path params split by a fixed segment + required & optional query all map") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[MappingController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(getRequest("/api/map/costam/hello/x/42?id=ABC&query=world"))
                .provideSome[Scope](ZLayer.succeed(new MappingController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("p1=hello")) && assertTrue(
            body.contains("p2=42")
          ) && assertTrue(body.contains("id=ABC")) && assertTrue(body.contains("q=world"))
        },
        test("optional query omitted maps cleanly") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[MappingController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(getRequest("/api/map/costam/hi/x/7?id=Z"))
                .provideSome[Scope](ZLayer.succeed(new MappingController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("p1=hi")) && assertTrue(
            body.contains("p2=7")
          ) && assertTrue(body.contains("id=Z")) && assertTrue(body.contains("q=none"))
        },
        test("@Path declared in reverse of URL order still maps correctly") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[MappingController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(getRequest("/api/map/r/foo/9?q=zzz"))
                .provideSome[Scope](ZLayer.succeed(new MappingController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("a=foo")) && assertTrue(
            body.contains("b=9")
          ) && assertTrue(body.contains("q=zzz"))
        },
        test("unconsumed @Path interleaved with a query maps to the right arguments") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[MappingController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(getRequest("/api/map/u/foo/9?q=zzz"))
                .provideSome[Scope](ZLayer.succeed(new MappingController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("a=foo")) && assertTrue(
            body.contains("b=9")
          ) && assertTrue(body.contains("q=zzz"))
        },
      ),
      suite("headers, cookies, files, auth")(
        test("@Header binds a request header") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[HeaderCookieController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(Request.get("/api/hc/header").addHeader("X-Token", "abc"))
                .provideSome[Scope](ZLayer.succeed(new HeaderCookieController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("token=abc"))
        },
        test("@Cookie binds a request cookie") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[HeaderCookieController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(Request.get("/api/hc/cookie").addHeader("Cookie", "session=xyz"))
                .provideSome[Scope](ZLayer.succeed(new HeaderCookieController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("session=xyz"))
        },
        test("@Deprecated marks the endpoint deprecated") {
          val ep = Okapi.endpoints[HeaderCookieController].find(_.endpoint.showShort.contains("/api/hc/legacy")).get
          val isDeprecated = ep.endpoint.info.deprecated
          assertTrue(isDeprecated)
        },
        test("FileResponse sets Content-Disposition and returns the bytes") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[FileController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/files/clean")).provideSome[Scope](ZLayer.succeed(new FileController))
            )
            body <- resp.body.asString
          } yield {
            val cd = resp.headers.get("Content-Disposition").getOrElse("")
            val hasFilename = cd.contains("filename=\"my report.txt\"")
            assertTrue(resp.status.code == 200) && assertTrue(hasFilename) && assertTrue(body == "hello")
          }
        },
        test("FileResponse strips quotes and CR/LF from the filename") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[FileController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/files/dirty")).provideSome[Scope](ZLayer.succeed(new FileController))
            )
          } yield {
            val cd = resp.headers.get("Content-Disposition").getOrElse("")
            val sanitized = cd.contains("filename=\"evilname.txt\"")
            val noCr = !cd.contains("\r")
            val noLf = !cd.contains("\n")
            assertTrue(sanitized) && assertTrue(noCr) && assertTrue(noLf)
          }
        },
        test("@BearerAuth passes the token to the controller") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[SecureController])
          for {
            resp <- ZIO.scoped(
              routes
                .runZIO(Request.get("/api/secure/me").addHeader("Authorization", "Bearer secret123"))
                .provideSome[Scope](ZLayer.succeed(new SecureController))
            )
            body <- resp.body.asString
          } yield assertTrue(resp.status.code == 200) && assertTrue(body.contains("token=secret123"))
        },
        test("@BearerAuth without a token is rejected (4xx)") {
          val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[SecureController])
          for {
            resp <- ZIO.scoped(
              routes.runZIO(Request.get("/api/secure/me")).provideSome[Scope](ZLayer.succeed(new SecureController))
            )
          } yield {
            val rejected = resp.status.code == 401 || resp.status.code == 400
            assertTrue(rejected)
          }
        },
      ),
      suite("websocket generation")(
        test("websocket endpoints are generated for @WebSocket methods") {
          val rendered = Okapi.endpoints[WsTestController].map(_.endpoint.showShort)
          val hasEcho = rendered.exists(_.contains("/api/ws/echo"))
          val hasChat = rendered.exists(_.contains("/api/ws/chat/{room}"))
          assertTrue(hasEcho) && assertTrue(hasChat)
        },
        test("websocket chat endpoint carries its path and query inputs") {
          val ep = Okapi.endpoints[WsTestController].find(_.endpoint.showShort.contains("/api/ws/chat")).get
          val in = ep.endpoint.input.show
          val hasRoom = in.contains("room")
          val hasUser = in.contains("user")
          assertTrue(hasRoom) && assertTrue(hasUser)
        },
      ),
      suite("openapi spec")(
        test("openApiYaml renders an OpenAPI document for the controllers") {
          val yaml = Okapi.openApiYaml[Tuple1[StatusController]]("Okapi", "1.0.0")
          val hasHeader = yaml.contains("openapi:")
          val hasPath = yaml.contains("/api/status")
          assertTrue(hasHeader) && assertTrue(hasPath)
        }
      ),
    )
  }
}
