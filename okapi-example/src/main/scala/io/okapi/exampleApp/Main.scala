package io.okapi.exampleApp

import zio.*
import zio.http.{ Response as ZioHttpResponse, Routes, Server }
import zio.json.JsonCodec

import io.okapi.core.annotations.{ Controller, Get, Post, Query, Path, Header, RequestBody, Produces, Consumes, Description, Summary, Tag }
import io.okapi.core.http.ApiError
import io.okapi.core.Okapi

final case class StatusDto(ok: Boolean, message: String) derives JsonCodec

final case class EchoReq(text: String) derives JsonCodec

final case class EchoResp(echo: String, id: Int) derives JsonCodec

@Controller("/api/example")
@Tag("Example")
final class ExampleController {

  @Get("/hello")
  @Summary("Hello endpoint")
  @Description("Returns simple greeting")
  def sayHello(
    @Query("name") name: Option[String]
  ): String =
    s"Hello, ${name.getOrElse("World")}!"

  @Get("/status")
  @Produces("application/json")
  def getStatus: StatusDto =
    StatusDto(ok = true, message = "OK")

  @Post("/echo")
  @Consumes("application/json")
  @Produces("application/json")
  def echo(
    @Path("id") id: Int,
    @Header("X-Request-Id") reqId: Option[String],
    @RequestBody body: EchoReq,
  ): IO[ApiError, EchoResp] = {
    if body.text.trim.nn.isEmpty then ZIO.fail(ApiError.BadRequest("text cannot be empty"))
    else ZIO.succeed(EchoResp(echo = s"${body.text} (rid=${reqId.getOrElse("none")})", id = id))
  }
}

object Main extends ZIOAppDefault {

  private type Controllers = (ExampleController, AdminController)

  private val defaultPort = 38081

  private val serverPort =
    sys.env.get("OKAPI_EXAMPLE_PORT").flatMap(_.toIntOption).getOrElse(defaultPort)

  private val apiRoutes: Routes[Okapi.Environment[Controllers], ZioHttpResponse] =
    Okapi.routes[Controllers]

  private val swagger: Routes[Any, ZioHttpResponse] =
    Okapi.swagger[Controllers]("Okapi Example API", "0.0.1")

  private val app = apiRoutes ++ swagger

  override def run: ZIO[Any, Throwable, Unit] =
    Okapi.registerOkapiControllers[Controllers](
      Server
        .serve(app)
        .provideSome[Okapi.Environment[Controllers]](
          ZLayer.succeed(Server.Config.default.port(serverPort)),
          Server.live,
        ),
    )
}
