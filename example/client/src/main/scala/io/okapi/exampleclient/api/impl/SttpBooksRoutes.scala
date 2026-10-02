package io.okapi.exampleclient.api.impl

import zio.Task

import com.github.plokhotnyuk.jsoniter_scala.core.{ writeToArray, JsonValueCodec }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.okapi.exampleclient.api.BooksRoutes
import io.okapi.exampleclient.api.models.{ Book, BookReview, BookStats, CreateBookRequest }
import sttp.model.MediaType
import sttp.model.Uri.UriContext

/** The sttp implementation of [[BooksRoutes]]. */
final class SttpBooksRoutes(transport: SttpTransport) extends BooksRoutes[Task] {
  import transport.{ asBody, baseUri, request }

  private given listBookCodec: JsonValueCodec[List[Book]] = JsonCodecMaker.make
  private given listBookReviewCodec: JsonValueCodec[List[BookReview]] = JsonCodecMaker.make

  override def listBooks(genre: Option[String], limit: Option[Int]): Task[List[Book]] =
    transport.json[List[Book]](request.get(uri"$baseUri/api/books?genre=$genre&limit=$limit").response(asBody))

  override def createBook(createBookRequest: CreateBookRequest): Task[Book] =
    transport.json[Book](
      request
        .post(uri"$baseUri/api/books")
        .body(writeToArray(createBookRequest))
        .contentType(MediaType.ApplicationJson)
        .response(asBody)
    )

  override def stats(): Task[BookStats] =
    transport.json[BookStats](request.get(uri"$baseUri/api/books/stats").response(asBody))

  override def getBook(id: Int): Task[Book] =
    transport.json[Book](request.get(uri"$baseUri/api/books/$id").response(asBody))

  override def updateBook(id: Int, createBookRequest: CreateBookRequest): Task[Book] =
    transport.json[Book](
      request
        .put(uri"$baseUri/api/books/$id")
        .body(writeToArray(createBookRequest))
        .contentType(MediaType.ApplicationJson)
        .response(asBody)
    )

  override def deleteBook(id: Int): Task[Unit] =
    transport.unit(request.delete(uri"$baseUri/api/books/$id").response(asBody))

  override def getReviews(id: Int, limit: Option[Int]): Task[List[BookReview]] =
    transport.json[List[BookReview]](request.get(uri"$baseUri/api/books/$id/reviews?limit=$limit").response(asBody))
}
