package io.okapi.core

import io.okapi.core.http.{ ApiError, ApiErrorException }
import sttp.monad.MonadError

/** The effect `F[_]` controller methods return. A method may return `F[A]` or a plain `A` (lifted with [[pure]]). */
trait OkapiEffect[F[_]] {
  def monad: MonadError[F]

  def pure[A](value: => A): F[A] = monad.eval(value)

  /** Fails with `error`; [[attempt]] surfaces it as a `Left`. */
  def fail[A](error: ApiError): F[A] = monad.error(ApiErrorException(error))

  /** Surfaces an [[ApiError]] raised in `fa` as a `Left`; any other failure stays in `F`. */
  def attempt[A](fa: F[A]): F[Either[ApiError, A]]
}

object OkapiEffect {

  def apply[F[_]](using effect: OkapiEffect[F]): OkapiEffect[F] = effect

  /** For effects whose error channel is `Throwable` (`Try`, `Future`, ...): an [[ApiError]] travels through `F` wrapped
    * in [[ApiErrorException]]; any other failure is left untouched.
    */
  def fromMonadError[F[_]](using m: MonadError[F]): OkapiEffect[F] = {
    new OkapiEffect[F] {
      val monad: MonadError[F] = m

      def attempt[A](fa: F[A]): F[Either[ApiError, A]] = {
        m.handleError(m.map(fa)(a => Right(a): Either[ApiError, A])) {
          case ApiErrorException(error) =>
            m.unit(Left(error))
        }
      }
    }
  }
}
