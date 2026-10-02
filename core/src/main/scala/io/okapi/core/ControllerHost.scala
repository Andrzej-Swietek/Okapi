package io.okapi.core

import io.okapi.core.http.ApiError
import sttp.monad.MonadError

/** How generated server logic reaches controller `C` and runs its `F` effects inside the server effect `G`: with a
  * fixed instance ([[ControllerHost.instance]], `G = F`), or by resolving the controller per request.
  */
trait ControllerHost[C, F[_], G[_]] {
  def effect: OkapiEffect[F]

  def monad: MonadError[G]

  def run[A](call: C => F[A]): G[Either[ApiError, A]]
}

object ControllerHost {

  /** Serves a fixed controller instance in its own effect `F`. */
  def instance[C, F[_]](controller: C)(using e: OkapiEffect[F]): ControllerHost[C, F, F] = {
    new ControllerHost[C, F, F] {
      val effect: OkapiEffect[F] = e
      val monad: MonadError[F] = e.monad

      def run[A](call: C => F[A]): F[Either[ApiError, A]] = e.attempt(e.monad.suspend(call(controller)))
    }
  }
}
