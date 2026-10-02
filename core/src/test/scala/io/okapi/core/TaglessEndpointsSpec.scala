package io.okapi.core

import zio.test.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.okapi.core.annotations.{ Controller, Get, Path, Post, Query, RequestBody }
import io.okapi.core.http.ApiError
import io.okapi.core.http.ApiError.ApiErrorResponse
import scala.util.{ Failure, Success, Try }
import sttp.model.{ Method, StatusCode }
import sttp.monad.TryMonad
import sttp.tapir.generic.auto.*
import sttp.tapir.server.ServerEndpoint

/** okapi-core without ZIO: a tagless-final controller instantiated with `Try`, invoked through Tapir's server logic. */
object TaglessEndpointsSpec extends ZIOSpecDefault {

  final case class Book(id: Int, title: String)
  final case class NewBook(title: String)

  given JsonValueCodec[Book] = JsonCodecMaker.make
  given JsonValueCodec[NewBook] = JsonCodecMaker.make
  given JsonValueCodec[List[Book]] = JsonCodecMaker.make

  @Controller("/books")
  final class BookApi[F[_]](using F: OkapiEffect[F]) {

    @Get("/{id}")
    def get(@Path("id") id: Int): F[Book] =
      if id == 1 then F.pure(Book(1, "Dune")) else F.fail(ApiError.NotFound(s"book $id"))

    @Get("/search")
    def search(@Query("q") q: String): List[Book] =
      List(Book(1, q))

    @Post("/{id}/copies")
    def copy(@Path("id") id: Int, @RequestBody book: NewBook): F[Book] =
      F.pure(Book(id, book.title))

    @Get("/boom")
    def boom(): F[Book] =
      F.monad.error(new IllegalStateException("boom"))
  }

  private given OkapiEffect[Try] = OkapiEffect.fromMonadError[Try](using TryMonad)

  private val endpoints: List[ServerEndpoint[Any, Try]] = OkapiEndpoints[Try].of(BookApi[Try]())

  private def endpoint(path: String): ServerEndpoint[Any, Try] =
    endpoints.find(_.endpoint.showPathTemplate(showQueryParam = None) == path).get

  /** Runs the endpoint's server logic directly with the given (already decoded) input. */
  private def run(path: String, input: Any): Try[Either[Any, Any]] = {
    val se = endpoint(path)
    se.logic(TryMonad)(().asInstanceOf[se.PRINCIPAL])(input.asInstanceOf[se.INPUT])
  }

  override def spec: Spec[Any, Any] = {
    suite("TaglessEndpointsSpec")(
      test("generates one endpoint per routed method, most specific path first") {
        val paths = endpoints.map(e =>
          e.endpoint.method.map(_.method).getOrElse("?") -> e.endpoint.showPathTemplate(showQueryParam = None)
        )
        assertTrue(
          paths == List(
            "GET" -> "/books/boom",
            "GET" -> "/books/search",
            "GET" -> "/books/{id}",
            "POST" -> "/books/{id}/copies",
          )
        )
      },
      test("F[A] result is served as the success output") {
        assertTrue(run("/books/{id}", 1) == Success(Right(Book(1, "Dune"))))
      },
      test("ApiError raised in F becomes the (status, body) error output") {
        assertTrue(
          run("/books/{id}", 7) == Success(Left((StatusCode.NotFound, ApiErrorResponse(404, "book 7"))))
        )
      },
      test("plain A result is lifted with OkapiEffect.pure") {
        assertTrue(run("/books/search", "x") == Success(Right(List(Book(1, "x")))))
      },
      test("path params and body are passed in declaration order; POST defaults to 201") {
        val se = endpoint("/books/{id}/copies")
        val isPost = se.endpoint.method == Some(Method.POST)
        val created = se.endpoint.output.show.contains("201")
        val result = run("/books/{id}/copies", (3, NewBook("Emma")))
        assertTrue(isPost, created, result == Success(Right(Book(3, "Emma"))))
      },
      test("non-ApiError failures stay failures of F") {
        val message = run("/books/boom", ()) match {
          case Failure(e) => e.getMessage
          case other => other.toString
        }
        assertTrue(message == "boom")
      },
    )
  }
}
