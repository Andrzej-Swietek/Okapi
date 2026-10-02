package io.okapi.core

import sttp.tapir.server.ServerEndpoint

/** Effect-agnostic entry point: Tapir server endpoints for a controller instance whose methods return `F[A]`.
  *
  * {{{
  * given OkapiEffect[IO] = OkapiEffect.fromMonadError[IO]   // e.g. with tapir-cats' MonadError[IO]
  * val endpoints: List[ServerEndpoint[Any, IO]] = OkapiEndpoints[IO].of(BookController[IO](service))
  * }}}
  * Interpret the result with any Tapir server interpreter for `F` (http4s, Netty, ...).
  */
object OkapiEndpoints {

  inline def apply[F[_]]: ForEffect[F] = new ForEffect[F]

  final class ForEffect[F[_]] {
    inline def of[C](controller: C)(using effect: OkapiEffect[F]): List[ServerEndpoint[Any, F]] =
      macros.AnnotationProcessor.endpoints[C, F, F, Any](ControllerHost.instance[C, F](controller))
  }
}
