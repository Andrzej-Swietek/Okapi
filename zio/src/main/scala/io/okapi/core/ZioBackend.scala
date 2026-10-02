package io.okapi.core

import zio.{ Cause, RIO, Scope, Tag, ZIO }

import io.okapi.core.http.{ ApiError, ApiErrorException }
import sttp.monad.MonadError
import sttp.tapir.ztapir.RIOMonadError

/** ZIO as one instance of the Okapi effect abstraction.
  *
  * A controller method may return a plain `A` or any [[Handler]]: `IO[ApiError, A]`, `UIO[A]`, `Task[A]`,
  * `ZIO[UserRepo, NotFound, A]`, `ZIO[Scope, Throwable, A]`, ... Failures are mapped by [[outcome]].
  */
object ZioBackend {

  /** What a ZIO controller method may return: any environment, failing with an [[ApiError]] and/or a `Throwable`. */
  type Handler[-R, +A] = ZIO[R, ApiError | Throwable, A]

  /** Body message of the 500 sent for unexpected failures. The cause is logged, never sent to the client. */
  val InternalErrorMessage = "Internal server error"

  /** Classifies a handler's failure. An [[ApiError]] — failed directly, wrapped in [[ApiErrorException]], or died with
    * as `ApiErrorException` (e.g. after `.orDie`) — is returned as is. Any other failure or defect is logged and
    * becomes [[ApiError.Internal]] with [[InternalErrorMessage]], so exception messages never reach the client. Pure
    * interruption propagates unchanged.
    */
  def outcome[R, A](handler: Handler[R, A]): ZIO[R, Nothing, Either[ApiError, A]] = {
    handler.foldCauseZIO(
      cause => {
        apiErrorOf(cause) match {
          case Some(error) => ZIO.succeed(Left(error))
          case None if cause.isInterruptedOnly => ZIO.failCause(cause.stripFailures)
          case None =>
            ZIO
              .logErrorCause("Unhandled failure in an Okapi controller", cause)
              .as(Left(ApiError.Internal(InternalErrorMessage)))
        }
      },
      value => ZIO.succeed(Right(value)),
    )
  }

  private def apiErrorOf(cause: Cause[ApiError | Throwable]): Option[ApiError] = {
    cause
      .failureOption
      .collect {
        case error: ApiError => error
        case ApiErrorException(error) => error
      }
      .orElse(cause.dieOption.collect { case ApiErrorException(error) => error })
  }

  given zioEffect[R]: OkapiEffect[[x] =>> Handler[R, x]] with {
    val monad: MonadError[[x] =>> Handler[R, x]] = HandlerMonad[R]()

    override def pure[A](value: => A): Handler[R, A] = ZIO.succeed(value)

    def attempt[A](fa: Handler[R, A]): Handler[R, Either[ApiError, A]] = outcome(fa)
  }

  /** Resolves controller `C` from the ZIO environment on every request. The server effect is `RIO[C & Extra, *]`, where
    * `Extra` is everything the controller's methods require from the environment (`Any` when nothing).
    */
  sealed abstract class ZioHost[C: Tag, R, Extra]
    extends ControllerHost[C, [x] =>> Handler[R, x], [x] =>> RIO[C & Extra, x]] {
    val effect: OkapiEffect[[x] =>> Handler[R, x]] = zioEffect[R]
    val monad: MonadError[[x] =>> RIO[C & Extra, x]] = new RIOMonadError[C & Extra]

    protected def provide[A](effect: ZIO[R & C, Nothing, A]): ZIO[C & Extra, Nothing, A]

    def run[A](call: C => Handler[R, A]): RIO[C & Extra, Either[ApiError, A]] = {
      // suspended: a method that throws while building its effect is classified like any other failure
      provide(ZIO.serviceWithZIO[C](controller => outcome(ZIO.suspendSucceed(call(controller)))))
    }
  }

  /** For controllers whose methods need `C & Extra` from the environment. */
  final class EnvironmentHost[C: Tag, Extra] extends ZioHost[C, C & Extra, Extra] {
    protected def provide[A](effect: ZIO[C & Extra, Nothing, A]): ZIO[C & Extra, Nothing, A] = effect
  }

  /** For controllers with a method needing a `Scope`: every request runs in its own scope, closed once the method's
    * effect completes — so a scoped resource must not be used by a stream or WebSocket pipe returned from it.
    */
  final class ScopedEnvironmentHost[C: Tag, Extra] extends ZioHost[C, C & Extra & Scope, Extra] {
    protected def provide[A](effect: ZIO[C & Extra & Scope, Nothing, A]): ZIO[C & Extra, Nothing, A] =
      ZIO.scoped[C & Extra](effect)
  }

  final private class HandlerMonad[R] extends MonadError[[x] =>> Handler[R, x]] {
    def unit[T](t: T): Handler[R, T] = ZIO.succeed(t)
    def map[T, T2](fa: Handler[R, T])(f: T => T2): Handler[R, T2] = fa.map(f)
    def flatMap[T, T2](fa: Handler[R, T])(f: T => Handler[R, T2]): Handler[R, T2] = fa.flatMap(f)
    def error[T](t: Throwable): Handler[R, T] = ZIO.fail(t)
    def ensure[T](f: Handler[R, T], e: => Handler[R, Unit]): Handler[R, T] = f.ensuring(e.ignore)

    protected def handleWrappedError[T](rt: Handler[R, T])(h: PartialFunction[Throwable, Handler[R, T]])
      : Handler[R, T] =
      rt.catchSome(Function.unlift((e: ApiError | Throwable) => h.lift(asThrowable(e))))

    private def asThrowable(e: ApiError | Throwable): Throwable = {
      e match {
        case t: Throwable => t
        case error: ApiError => ApiErrorException(error)
      }
    }
  }
}
