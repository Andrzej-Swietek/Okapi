package io.okapi.core

import scala.compiletime.erasedValue

import zio.{ Task, ZIO, ZLayer }
import zio.http.{ Response, Routes }

import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import sttp.tapir.ztapir.ZServerEndpoint

object Okapi {

  final class RegisterControllersPartiallyApplied[Types <: Tuple] {
    inline def apply[R, E, A](
      effect: ZIO[Environment[Types] & R, E, A],
    ): ZIO[R, E, A] =
      effect.provideSomeLayer[R](
        controllerLayers[Types]
          .asInstanceOf[ZLayer[R, Nothing, Environment[Types]]],
      )
  }

  final class RegisterServicesPartiallyApplied[Types <: Tuple] {
    inline def apply[R, E, A](
      effect: ZIO[Environment[Types] & R, E, A],
    ): ZIO[R, E, A] =
      effect.provideSomeLayer[R](
        serviceLayers[Types]
          .asInstanceOf[ZLayer[R, Nothing, Environment[Types]]],
      )
  }

  type Environment[Types <: Tuple] = Types match {
    case EmptyTuple => Any
    case head *: tail => head & Environment[tail]
  }

  inline def endpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    macros.AnnotationProcessor.endpoints[T]

  inline def generateEndpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    macros.AnnotationProcessor.endpoints[T]

  inline def swaggerRoutes[T](
    title: String,
    version: String,
  ): Routes[Any, Response] =
    macros.AnnotationProcessor.swaggerRoutes[T](title, version)

  inline def httpRoutes[T]: Routes[T, Response] =
    macros.AnnotationProcessor.httpRoutes[T]

  transparent inline def layer[T] =
    ZLayer.derive[T]

  transparent inline def selectedEndpoints[Types <: Tuple]: List[ZServerEndpoint[Environment[Types], sttp.capabilities.WebSockets]] =
    inline erasedValue[Types] match {
      case _: (head *: EmptyTuple) =>
        endpoints[head].asInstanceOf[List[ZServerEndpoint[Environment[Types], sttp.capabilities.WebSockets]]]
      case _: (head *: tail) =>
        (endpoints[head] ++ selectedEndpoints[tail])
          .asInstanceOf[List[ZServerEndpoint[Environment[Types], sttp.capabilities.WebSockets]]]
    }

  transparent inline def routes[Types <: Tuple]: Routes[Environment[Types], Response] =
    inline erasedValue[Types] match {
      case _: (head *: EmptyTuple) =>
        httpRoutes[head].asInstanceOf[Routes[Environment[Types], Response]]
      case _: (head *: tail) =>
        (httpRoutes[head] ++ routes[tail]).asInstanceOf[Routes[Environment[Types], Response]]
    }

  inline def swagger[Types <: Tuple](
    title: String,
    version: String,
  ): Routes[Any, Response] = {
    val publicEndpoints = selectedEndpoints[Types].map(_.endpoint)
    ZioHttpInterpreter().toHttp(SwaggerInterpreter().fromEndpoints[Task](publicEndpoints, title, version))
  }

  transparent inline def controllerLayers[Types <: Tuple] =
    macros.AnnotationProcessor.controllerLayers[Types]

  transparent inline def serviceLayers[Types <: Tuple] =
    macros.AnnotationProcessor.controllerLayers[Types]

  transparent inline def autoLayer[Types <: Tuple]: ZLayer[Any, Nothing, Environment[Types]] =
    macros.AnnotationProcessor.autoLayer[Types].asInstanceOf[ZLayer[Any, Nothing, Environment[Types]]]

  inline def registerOkapiControllers[Types <: Tuple] =
    new RegisterControllersPartiallyApplied[Types]

  inline def registerOkapiServices[Types <: Tuple] =
    new RegisterServicesPartiallyApplied[Types]
}
