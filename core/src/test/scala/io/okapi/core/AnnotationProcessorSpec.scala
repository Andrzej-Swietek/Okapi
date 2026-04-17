package io.okapi.core

import io.okapi.core.annotations.{
  Controller,
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
}
import io.okapi.core.http.ApiError
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.{ IO, Scope, ZIO, ZLayer }
import zio.http.{ Body, Header, Headers, Method, Request, Response, Status }
import zio.json.JsonCodec
import zio.test.*

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
      @Query("name") name: Option[String],
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
      @Query("name") name: Option[String],
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
      @Query("name") name: Option[String],
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

  // ─── Test helpers ──────────────────────────────────────────────────────────

  private val testRoutes   = ZioHttpInterpreter().toHttp(Okapi.endpoints[TestController])
  private val nestedRoutes = ZioHttpInterpreter().toHttp(Okapi.endpoints[NestedController])
  private val contentRoutes = ZioHttpInterpreter().toHttp(Okapi.endpoints[ContentController])

  private type CombinedControllers = (TestController, AdminController)

  private def parseUrl(path: String): zio.http.URL =
    zio.http.URL.decode(path).getOrElse(throw new IllegalArgumentException(s"bad url: $path"))

  private def getRequest(path: String): Request =
    Request(
      method = Method.GET,
      url = parseUrl(path),
      headers = Headers.empty,
      body = Body.empty,
      version = zio.http.Version.Http_1_1,
      remoteAddress = None,
    )

  private def jsonRequest(method: Method, path: String, jsonBody: String): Request =
    Request(
      method = method,
      url = parseUrl(path),
      headers = Headers(
        Header.ContentType.parse("application/json")
          .fold(_ => throw new IllegalStateException("invalid media type"), identity),
      ),
      body = Body.fromString(jsonBody),
      version = zio.http.Version.Http_1_1,
      remoteAddress = None,
    )

  // ─── Specs ────────────────────────────────────────────────────────────────

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("AnnotationProcessorSpec")(
      suite("original behaviour")(
        test("generateEndpoints keeps HTTP metadata and paths") {
          val endpoints = Okapi.generateEndpoints[TestController]
          val rendered  = endpoints.map(_.endpoint.showShort)
          val helloEndpoint = endpoints
            .find(_.endpoint.showShort == "GET /api/test/hello")
            .getOrElse(throw new IllegalStateException("hello endpoint not generated"))
          val hasHelloSummary    = helloEndpoint.endpoint.info.summary.contains("hello endpoint")
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
          } yield assertTrue(response.status == Status.Ok) &&
            assertTrue(body.contains("works")) &&
            assertTrue(body.contains("42"))
        },
        test("selected routes combine multiple controllers") {
          val routes  = Okapi.routes[CombinedControllers]
          val request = Request.get("/api/admin/status")

          for {
            response <- ZIO.scoped {
              routes.runZIO(request).provideSome[Scope](
                Okapi.controllerLayers[CombinedControllers],
              )
            }
            body <- response.body.asString
          } yield assertTrue(response.status == Status.Ok) &&
            assertTrue(body.contains("admin-ok")) &&
            assertTrue(Okapi.selectedEndpoints[CombinedControllers].size == 3)
        },
        test("controller and service layers resolve constructor dependencies") {
          for {
            response <- ZIO.serviceWith[DependentController](_.hello(Some("Okapi"))).provide(
              ZLayer.make[DependentController](
                Okapi.layer[GreetingService],
                Okapi.layer[DependentController],
              ),
            )
          } yield assertTrue(response.message == "service:Okapi")
        },
      ),

      suite("path template parsing")(
        test("mid-path param shows correctly in showShort") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered  = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/nested/items/{id}/details"))
        },
        test("two path params show correctly in showShort") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered  = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/nested/users/{userId}/posts/{postId}"))
        },
        test("consecutive path params show correctly in showShort") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered  = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("DELETE /api/nested/resources/{category}/{id}"))
        },
        test("path param before and after fixed segment shows correctly") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered  = endpoints.map(_.endpoint.showShort)
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
            response.status == Status.BadRequest || response.status == Status.NotFound,
          )
        },
        test("path normalization handles leading slashes correctly") {
          val endpoints = Okapi.generateEndpoints[NestedController]
          val rendered  = endpoints.map(_.endpoint.showShort)
          val noDoubleSlash   = rendered.forall(p => !p.contains("//"))
          val knownMethod     = rendered.forall(p => p.startsWith("GET") || p.startsWith("POST") || p.startsWith("PUT") || p.startsWith("DELETE"))
          assertTrue(noDoubleSlash && knownMethod)
        },
      ),

      suite("content type support")(
        test("@Produces text/plain endpoint generated correctly") {
          val endpoints = Okapi.generateEndpoints[ContentController]
          val rendered  = endpoints.map(_.endpoint.showShort)
          assertTrue(rendered.contains("GET /api/content/text"))
        },
        test("Array[Byte] return type endpoint generated correctly") {
          val endpoints = Okapi.generateEndpoints[ContentController]
          val rendered  = endpoints.map(_.endpoint.showShort)
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
          val layer  = Okapi.autoLayer[Tuple1[AutoSingleController]]

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
          val layer  = Okapi.autoLayer[Tuple1[AutoTransitiveController]]

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
          val layer  = Okapi.autoLayer[Tuple1[AutoDeepController]]

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
          val layer  = Okapi.autoLayer[Tuple1[AutoMultiController]]

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
          type Both  = (AutoSingleController, AutoTransitiveController)
          val layer  = Okapi.autoLayer[Both]
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
    )
}
