package io.okapi.core

import io.okapi.core.annotations.{ Controller, Get, Path, Post, Query, RequestBody, Summary, Tag }
import io.okapi.core.http.ApiError
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import zio.{ IO, Scope, ZIO, ZLayer }
import zio.http.{ Body, Header, Headers, Request, Response, Status }
import zio.json.JsonCodec
import zio.test.*

object AnnotationProcessorSpec extends ZIOSpecDefault {

  final case class HelloResponse(message: String) derives JsonCodec
  final case class EchoRequest(text: String) derives JsonCodec
  final case class EchoResponse(id: Int, text: String) derives JsonCodec
  final case class AdminResponse(message: String) derives JsonCodec

  final class GreetingService {
    def greet(name: String): String = s"service:$name"
  }

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

  private val routes = ZioHttpInterpreter().toHttp(Okapi.endpoints[TestController])
  private type CombinedControllers = (TestController, AdminController)

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("AnnotationProcessorSpec")(
      test("generateEndpoints keeps HTTP metadata and paths") {
        val endpoints = Okapi.generateEndpoints[TestController]
        val rendered = endpoints.map(_.endpoint.showShort)
        val helloEndpoint = endpoints.find(_.endpoint.showShort == "GET /api/test/hello").getOrElse(
          throw new IllegalStateException("hello endpoint not generated"),
        )
        val hasHelloSummary = helloEndpoint.endpoint.info.summary.contains("hello endpoint")
        val hasHelloQueryInput = helloEndpoint.endpoint.input.show.contains("name")

        assertTrue(endpoints.size == 2) &&
        assertTrue(rendered.contains("GET /api/test/hello")) &&
        assertTrue(rendered.contains("POST /api/test/echo/{id}")) &&
        assertTrue(hasHelloSummary) &&
        assertTrue(hasHelloQueryInput)
      },
      test("generated routes execute controller methods through zio-http") {
        val request = Request
          .post(
            "/api/test/echo/42",
            Body.fromString("""{"text":"works"}"""),
          )
          .updateHeaders(
            _ ++ Headers(
              Header.ContentType.parse("application/json").fold(_ => throw new IllegalStateException("invalid media type"), identity),
            ),
          )

        for {
          response <- ZIO.scoped {
            routes.runZIO(request).provideSome[Scope](ZLayer.succeed(new TestController))
          }
          body <- response.body.asString
        } yield assertTrue(response.status == Status.Ok) &&
          assertTrue(body.contains("works")) &&
          assertTrue(body.contains("42"))
      },
      test("selected routes combine multiple controllers") {
        val routes = Okapi.routes[CombinedControllers]
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
    )
}
