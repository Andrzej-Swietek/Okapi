package io.okapi.core

import io.okapi.core.http.{ ApiError, ApiErrorException }
import sttp.monad.MonadError

/** The effect `F[_]` controller methods return. A method may return `F[A]` or a plain `A` (lifted with [[pure]]).
  *
  * The only Okapi-specific capability is [[attempt]]: surfacing an [[ApiError]] raised in `F` as a `Left`.
  */
trait OkapiEffect[F[_]] {
  def monad: MonadError[F]

  def pure[A](value: => A): F[A] = monad.eval(value)

  def fail[A](error: ApiError): F[A] = monad.error(ApiErrorException(error))

  def attempt[A](fa: F[A]): F[Either[ApiError, A]]
}

object OkapiEffect {

  def apply[F[_]](using effect: OkapiEffect[F]): OkapiEffect[F] = effect

  /** For effects whose error channel is `Throwable` (cats-effect `IO`, `Future`, `Try`, ...): an [[ApiError]] travels
    * through `F` wrapped in [[ApiErrorException]]; any other failure is left untouched.
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
