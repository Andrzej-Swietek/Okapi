package io.okapi.client

import io.okapi.core.OkapiEffect
import sttp.client4.Backend
import sttp.model.Uri

/** HTTP clients for Okapi API traits, in any effect `F`:
  * {{{
  * @Controller("/api/books")
  * trait BookApi[F[_]] {
  *   @Get("/{id}") def get(@Path("id") id: Int): F[Book]
  * }
  *
  * val books: BookApi[IO] = OkapiClient[IO].of[BookApi[IO]](uri"http://localhost:8080", backend)
  * }}}
  * A routed method must return `F[A]` (a compile error otherwise); calling an abstract method that is not routed throws
  * `UnsupportedOperationException`. The requests are built from the same annotations as the server's endpoints; an
  * error response fails `F` with the [[io.okapi.core.http.ApiError]] for its status. A checked exception thrown by the
  * call itself (e.g. `sttp.client4.SttpClientException` from a backend whose effect throws) reaches the caller as the
  * cause of a `java.lang.reflect.UndeclaredThrowableException`.
  */
object OkapiClient {

  inline def apply[F[_]]: ForEffect[F] = new ForEffect[F]

  final class ForEffect[F[_]] {
    inline def of[Api](baseUri: Uri, backend: Backend[F])(using effect: OkapiEffect[F]): Api =
      ${ ClientMacro.of[F, Api]('baseUri, 'backend, 'effect) }
  }
}
