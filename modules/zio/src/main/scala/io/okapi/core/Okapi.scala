package io.okapi.core

import zio.{ Task, ZIO, ZLayer }
import zio.http.{ Response, Routes }

import io.okapi.openapi.OkapiDocs
import sttp.capabilities.WebSockets
import sttp.tapir.AnyEndpoint
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }

object Okapi {

  final class RegisterControllersPartiallyApplied[Types <: Tuple] {
    inline def apply[R, E, A](
      effect: ZIO[Environment[Types] & R, E, A]
    ): ZIO[R, E, A] = {
      effect.provideSomeLayer[R](
        controllerLayers[Types].asInstanceOf[ZLayer[R, Nothing, Environment[Types]]]
      )
    }
  }

  final class RegisterServicesPartiallyApplied[Types <: Tuple] {
    inline def apply[R, E, A](
      effect: ZIO[Environment[Types] & R, E, A]
    ): ZIO[R, E, A] = {
      effect.provideSomeLayer[R](
        serviceLayers[Types].asInstanceOf[ZLayer[R, Nothing, Environment[Types]]]
      )
    }
  }

  type Environment[Types <: Tuple] = Types match {
    case EmptyTuple => Any
    case head *: tail => head & Environment[tail]
  }

  /** Endpoints of controller `T`, typed `List[ZServerEndpoint[T & R, WebSockets]]` where `R` is everything its methods
    * need from the ZIO environment. The controller is resolved from the environment on every request.
    */
  transparent inline def endpoints[T]: List[ServerEndpoint[WebSockets, ?]] =
    macros.ZioAnnotationProcessor.endpoints[T]

  /** Same as [[endpoints]]. */
  transparent inline def generateEndpoints[T]: List[ServerEndpoint[WebSockets, ?]] =
    endpoints[T]

  /** `Routes[T & R, Response]` for controller `T` (see [[endpoints]]), interpreted with `options` — e.g.
    * `ZioHttpServerOptions.customiseInterceptors.corsInterceptor(CORSInterceptor.default).options`.
    */
  transparent inline def httpRoutes[T](options: ZioHttpServerOptions[Any]): Routes[Nothing, Response] =
    macros.ZioAnnotationProcessor.httpRoutes[T](options)

  /** [[httpRoutes]] with Tapir's default server options. */
  transparent inline def httpRoutes[T]: Routes[Nothing, Response] =
    macros.ZioAnnotationProcessor.httpRoutes[T](ZioHttpServerOptions.default[Any])

  /** Swagger UI (`/docs`) for controller `T` plus `extra` endpoints. */
  inline def swaggerRoutes[T](
    title: String,
    version: String,
    extra: List[AnyEndpoint] = Nil,
    customise: OkapiDocs.Customise = identity,
  ): Routes[Any, Response] =
    swaggerFor(endpoints[T].map(_.endpoint) ++ extra, title, version, customise)

  /** `ZLayer.derive[T]`. */
  transparent inline def layer[T] =
    ZLayer.derive[T]

  /** Endpoints of every controller in `Types`, typed with all their environment requirements. */
  transparent inline def selectedEndpoints[Types <: Tuple]: List[ServerEndpoint[WebSockets, ?]] =
    macros.ZioAnnotationProcessor.selectedEndpoints[Types]

  /** `Routes[C1 & C2 & ... & R, Response]` for the controllers in `Types` (see [[endpoints]]), interpreted with
    * `options` (see [[httpRoutes]]).
    */
  transparent inline def routes[Types <: Tuple](options: ZioHttpServerOptions[Any]): Routes[Nothing, Response] =
    macros.ZioAnnotationProcessor.routes[Types](options)

  /** [[routes]] with Tapir's default server options. */
  transparent inline def routes[Types <: Tuple]: Routes[Nothing, Response] =
    macros.ZioAnnotationProcessor.routes[Types](ZioHttpServerOptions.default[Any])

  /** Swagger UI (`/docs`) for the controllers in `Types` plus `extra` endpoints; `customise` edits the document (e.g.
    * [[OkapiDocs.withBearerAuth]]).
    */
  inline def swagger[Types <: Tuple](
    title: String,
    version: String,
    extra: List[AnyEndpoint] = Nil,
    customise: OkapiDocs.Customise = identity,
  ): Routes[Any, Response] =
    swaggerFor(selectedEndpoints[Types].map(_.endpoint) ++ extra, title, version, customise)

  /** The OpenAPI 3 document for the controllers in `Types` plus `extra` endpoints, as YAML. */
  inline def openApiYaml[Types <: Tuple](
    title: String,
    version: String,
    extra: List[AnyEndpoint] = Nil,
    customise: OkapiDocs.Customise = identity,
  ): String =
    OkapiDocs.yaml(selectedEndpoints[Types].map(_.endpoint) ++ extra, title, version, customise)

  /** The OpenAPI 3 document for the controllers in `Types` plus `extra` endpoints, as JSON. */
  inline def openApiJson[Types <: Tuple](
    title: String,
    version: String,
    extra: List[AnyEndpoint] = Nil,
    customise: OkapiDocs.Customise = identity,
  ): String =
    OkapiDocs.json(selectedEndpoints[Types].map(_.endpoint) ++ extra, title, version, customise)

  /** Derives a layer for each type in `Types`, wired with `ZLayer.make`; their dependencies are not derived (see
    * [[autoLayer]]).
    */
  transparent inline def controllerLayers[Types <: Tuple] =
    macros.ZioAnnotationProcessor.controllerLayers[Types]

  /** Same as [[controllerLayers]]. */
  transparent inline def serviceLayers[Types <: Tuple] =
    macros.ZioAnnotationProcessor.controllerLayers[Types]

  /** Derives every controller in `Types` and, transitively, their constructor dependencies. Dependencies that cannot be
    * derived (traits, abstract classes, library types such as `String` or `zio.http.Client`) become the input of the
    * returned layer, e.g. `ZLayer[Repo, Nothing, ...]`.
    */
  transparent inline def autoLayer[Types <: Tuple]: Any =
    macros.ZioAnnotationProcessor.autoLayer[Types]

  private def swaggerFor(
    endpoints: List[AnyEndpoint],
    title: String,
    version: String,
    customise: OkapiDocs.Customise,
  ): Routes[Any, Response] =
    ZioHttpInterpreter().toHttp(OkapiDocs.swagger[Task](endpoints, title, version, customise))

  /** Provides [[controllerLayers]] of `Types` to an effect: `registerOkapiControllers[(A, B)](program)`. */
  inline def registerOkapiControllers[Types <: Tuple] =
    new RegisterControllersPartiallyApplied[Types]

  /** Provides [[serviceLayers]] of `Types` to an effect. */
  inline def registerOkapiServices[Types <: Tuple] =
    new RegisterServicesPartiallyApplied[Types]
}
