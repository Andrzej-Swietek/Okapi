package io.okapi.client

import zio.test.*

import io.okapi.core.{ OkapiEffect, OkapiEndpoints }
import io.okapi.core.annotations.{ Controller, Delete, Get, Header, Path, Post, Query, RequestBody }
import io.okapi.core.http.{ ApiError, ApiErrorException, ApiResponse, FileResponse }
import io.okapi.core.json.JsoniterCodec
import scala.util.{ Failure, Success, Try }
import sttp.client4.testing.BackendStub
import sttp.model.{ StatusCode, Uri }
import sttp.monad.TryMonad
import sttp.tapir.server.stub4.TapirStubInterpreter

object OkapiClientSpec extends ZIOSpecDefault {

  final case class Book(id: Int, title: String) derives JsoniterCodec

  /** The API both sides share: the server implements it, the client is generated from it. */
  @Controller("/api/books")
  trait BookApi[F[_]] {

    @Get("/{id}")
    def get(@Path("id") id: Int): F[Book]

    @Get("")
    def search(@Query("q") q: Option[String], @Query("limit") limit: Int = 10): F[List[Book]]

    @Post("")
    def create(@RequestBody book: Book, @Header("X-Request-Id") requestId: String): F[Book]

    @Delete("/{id}")
    def delete(@Path("id") id: Int): F[Unit]

    @Get("/{id}/cover")
    def cover(@Path("id") id: Int): F[FileResponse]

    @Get("/{id}/status")
    def status(@Path("id") id: Int): F[ApiResponse[Book]]

    @Get("/wide")
    def wide(
      @Query("a1") a1: Int,
      @Query("a2") a2: Int,
      @Query("a3") a3: Int,
      @Query("a4") a4: Int,
      @Query("a5") a5: Int,
      @Query("a6") a6: Int,
      @Query("a7") a7: Int,
      @Query("a8") a8: Int,
      @Query("a9") a9: Int,
      @Query("a10") a10: Int,
      @Query("a11") a11: Int,
      @Query("a12") a12: Int,
      @Query("a13") a13: Int,
      @Query("a14") a14: Int,
      @Query("a15") a15: Int,
      @Query("a16") a16: Int,
      @Query("a17") a17: Int,
      @Query("a18") a18: Int,
      @Query("a19") a19: Int,
      @Query("a20") a20: Int,
      @Query("a21") a21: Int,
      @Query("a22") a22: Int,
      @Query("a23") a23: Int,
      @Query("a24") a24: Int,
      @Query("a25") a25: Int,
    ): F[String]
  }

  final class BookServer(using F: OkapiEffect[Try]) extends BookApi[Try] {
    def get(id: Int): Try[Book] = if id == 1 then F.pure(Book(1, "Dune")) else F.fail(ApiError.NotFound(s"book $id"))
    def search(q: Option[String], limit: Int): Try[List[Book]] = F.pure(List(Book(limit, q.getOrElse("all"))))
    def create(book: Book, requestId: String): Try[Book] = F.pure(book.copy(title = s"${book.title} ($requestId)"))
    def delete(id: Int): Try[Unit] = F.pure(())
    def cover(id: Int): Try[FileResponse] = F.pure(FileResponse(Array[Byte](1, 2), s"cover-$id.png"))
    def status(id: Int): Try[ApiResponse[Book]] =
      F.pure(ApiResponse(Book(id, "x")).withStatus(StatusCode.Accepted).withHeader("X-Trace", s"t$id"))
    def wide(
      a1: Int,
      a2: Int,
      a3: Int,
      a4: Int,
      a5: Int,
      a6: Int,
      a7: Int,
      a8: Int,
      a9: Int,
      a10: Int,
      a11: Int,
      a12: Int,
      a13: Int,
      a14: Int,
      a15: Int,
      a16: Int,
      a17: Int,
      a18: Int,
      a19: Int,
      a20: Int,
      a21: Int,
      a22: Int,
      a23: Int,
      a24: Int,
      a25: Int,
    ): Try[String] = {
      F.pure(
        List(
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          a20,
          a21,
          a22,
          a23,
          a24,
          a25,
        ).mkString(",")
      )
    }
  }

  private given OkapiEffect[Try] = OkapiEffect.fromMonadError[Try](using TryMonad)

  private val backend = {
    TapirStubInterpreter(BackendStub[Try](TryMonad))
      .whenServerEndpointsRunLogic(OkapiEndpoints[Try].of(BookServer()))
      .backend()
  }

  private val client: BookApi[Try] = OkapiClient[Try].of[BookApi[Try]](Uri.unsafeParse("http://books.local"), backend)

  override def spec = {
    suite("OkapiClientSpec")(
      test("path, query with a default, header and body arguments reach the server") {
        assertTrue(
          client.get(1) == Success(Book(1, "Dune")),
          client.search(Some("dune")) == Success(List(Book(10, "dune"))),
          client.search(None, limit = 3) == Success(List(Book(3, "all"))),
          client.create(Book(2, "Emma"), "req-7") == Success(Book(2, "Emma (req-7)")),
          client.delete(2) == Success(()),
        )
      },
      test("a file download comes back as a FileResponse") {
        val cover = client.cover(5).get
        assertTrue(cover.filename == "cover-5.png", cover.data.toList == List[Byte](1, 2))
      },
      test("an error response fails F with the ApiError for its status") {
        val error = client.get(9) match {
          case Failure(ApiErrorException(error)) => Some(error)
          case _ => None
        }
        assertTrue(error == Some(ApiError.NotFound("book 9")))
      },
      test("ApiResponse keeps the status and headers chosen by the server") {
        val response = client.status(4).get
        assertTrue(
          response.body == Book(4, "x"),
          response.status == Some(StatusCode.Accepted),
          response.headers.exists(h => h.name == "X-Trace" && h.value == "t4"),
        )
      },
      test("more than 22 parameters") {
        val result =
          client.wide(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
        assertTrue(result == Success((1 to 25).mkString(",")))
      },
      test("the client is a plain instance of the trait") {
        assertTrue(client.toString.contains("BookApi"), client == client)
      },
    )
  }
}
