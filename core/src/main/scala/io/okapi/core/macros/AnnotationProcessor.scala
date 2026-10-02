package io.okapi.core.macros

import io.okapi.core.ControllerHost
import scala.quoted.*
import sttp.tapir.server.ServerEndpoint

/** Entry point of the effect-agnostic endpoint macro ([[EndpointsMacro]]).
  *
  * `C` is the controller, `F` the effect its methods return, `G` the effect the server logic runs in and `R` the
  * capabilities of the resulting endpoints.
  */
private[okapi] object AnnotationProcessor {

  inline def endpoints[C, F[_], G[_], R](host: ControllerHost[C, F, G]): List[ServerEndpoint[R, G]] =
    ${ endpointsImpl[C, F, G, R]('host) }

  private def endpointsImpl[C: Type, F[_]: Type, G[_]: Type, R: Type](
    host: Expr[ControllerHost[C, F, G]]
  )(using q: Quotes
  ): Expr[List[ServerEndpoint[R, G]]] =
    EndpointsMacro(q).expand[C, F, G, R](host)
}
