package io.okapi.exampleApp

import sttp.tapir.generic.auto.*
import zio.*
import zio.http.{ Response as ZioHttpResponse, Routes, Server }

import io.okapi.core.Okapi

object Main extends ZIOAppDefault {

  private type Controllers = (BookController, UserController, ExploreController, CoverController, AdminController, WsController)

  private val serverPort =
    sys.env.get("OKAPI_EXAMPLE_PORT").flatMap(_.toIntOption).getOrElse(38081)

  private lazy val apiRoutes: Routes[Okapi.Environment[Controllers], ZioHttpResponse] =
    Okapi.routes[Controllers]

  private lazy val swagger: Routes[Any, ZioHttpResponse] =
    Okapi.swagger[Controllers]("Okapi Example API", "1.0.0")

  private def app = apiRoutes ++ swagger

  override def run: ZIO[Any, Throwable, Unit] =
    Server
      .serve(app)
      .provide(
        ZLayer.succeed(Server.Config.default.port(serverPort)),
        Server.live,
        Okapi.autoLayer[Controllers],
      )
}
