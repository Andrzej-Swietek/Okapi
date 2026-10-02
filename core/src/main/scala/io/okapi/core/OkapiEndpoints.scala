package io.okapi.core

import sttp.tapir.server.ServerEndpoint

/** Tapir server endpoints for a controller instance whose methods return `F[A]`:
  * {{{
  * given OkapiEffect[Try] = OkapiEffect.fromMonadError[Try](using sttp.monad.TryMonad)
  * val endpoints: List[ServerEndpoint[Any, Try]] = OkapiEndpoints[Try].of(BookController[Try]())
  * }}}
  * The result runs on any Tapir server interpreter for `F`.
  */
object OkapiEndpoints {

  inline def apply[F[_]]: ForEffect[F] = new ForEffect[F]

  final class ForEffect[F[_]] {

    /** One endpoint per routed method of `controller`, most specific path first. */
    inline def of[C](controller: C)(using effect: OkapiEffect[F]): List[ServerEndpoint[Any, F]] =
      macros.AnnotationProcessor.endpoints[C, F, F, Any](ControllerHost.instance[C, F](controller))
  }
}
