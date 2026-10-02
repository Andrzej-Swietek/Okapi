package io.okapi.core.macros

import zio.http.{ Response, Routes }

import io.okapi.core.macros.layers.LayerMacro
import scala.quoted.*
import sttp.capabilities.WebSockets
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ziohttp.ZioHttpServerOptions

/** Facade over the ZIO-specialised macros: endpoints ([[ZioEndpointsMacro]]) and layers ([[LayerMacro]]). All entry
  * points are `transparent`, so callers see the precise environment / layer types the macros compute.
  */
private[okapi] object ZioAnnotationProcessor {

  /** `List[ZServerEndpoint[C & R, WebSockets]]`, `R` being what `C`'s methods need from the environment. */
  transparent inline def endpoints[C]: List[ServerEndpoint[WebSockets, ?]] =
    ${ endpointsImpl[C] }

  /** Endpoints of every controller in `Types`, typed with all their requirements. */
  transparent inline def selectedEndpoints[Types <: Tuple]: List[ServerEndpoint[WebSockets, ?]] =
    ${ selectedEndpointsImpl[Types] }

  /** `Routes[C & R, Response]`. */
  transparent inline def httpRoutes[C](options: ZioHttpServerOptions[Any]): Routes[Nothing, Response] =
    ${ httpRoutesImpl[C]('options) }

  /** `Routes[C1 & C2 & ... & R, Response]`. */
  transparent inline def routes[Types <: Tuple](options: ZioHttpServerOptions[Any]): Routes[Nothing, Response] =
    ${ routesImpl[Types]('options) }

  transparent inline def controllerLayers[Types <: Tuple]: Any =
    ${ controllerLayersImpl[Types] }

  transparent inline def autoLayer[Types <: Tuple]: Any =
    ${ autoLayerImpl[Types] }

  private def endpointsImpl[C: Type](using q: Quotes): Expr[List[ServerEndpoint[WebSockets, ?]]] =
    ZioEndpointsMacro(q).controllerEndpoints[C].asExprOf[List[ServerEndpoint[WebSockets, ?]]]

  private def selectedEndpointsImpl[Types: Type](using q: Quotes): Expr[List[ServerEndpoint[WebSockets, ?]]] =
    ZioEndpointsMacro(q).selectedEndpoints[Types].asExprOf[List[ServerEndpoint[WebSockets, ?]]]

  private def httpRoutesImpl[C: Type](options: Expr[ZioHttpServerOptions[Any]])(using q: Quotes)
    : Expr[Routes[Nothing, Response]] =
    ZioEndpointsMacro(q).controllerRoutes[C](options).asExprOf[Routes[Nothing, Response]]

  private def routesImpl[Types: Type](options: Expr[ZioHttpServerOptions[Any]])(using q: Quotes)
    : Expr[Routes[Nothing, Response]] =
    ZioEndpointsMacro(q).selectedRoutes[Types](options).asExprOf[Routes[Nothing, Response]]

  private def controllerLayersImpl[Types: Type](using q: Quotes): Expr[Any] = {
    val layers = LayerMacro(q)
    layers.expand[Types](layers.ExplicitTypes)
  }

  private def autoLayerImpl[Types: Type](using q: Quotes): Expr[Any] = {
    val layers = LayerMacro(q)
    layers.expand[Types](layers.ConstructorGraph)
  }
}
