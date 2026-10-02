package io.okapi.exampleclient.api.impl

import zio.Task
import zio.stream.ZStream

import io.okapi.exampleclient.api.CoversRoutes
import io.okapi.exampleclient.api.models.BookCoverDto
import sttp.model.Uri.UriContext

/** The sttp implementation of [[CoversRoutes]]. */
final class SttpCoversRoutes(transport: SttpTransport) extends CoversRoutes[Task] {
  import transport.{ asBody, baseUri, request, streams }

  override def downloadCover(bookId: Int): Task[Array[Byte]] =
    transport.bytes(request.get(uri"$baseUri/api/covers/$bookId").response(asBody))

  override def uploadCover(bookId: Int, body: Array[Byte], title: Option[String], altText: Option[String])
    : Task[BookCoverDto] =
    transport.json[BookCoverDto](
      request
        .post(uri"$baseUri/api/covers/$bookId?title=$title&altText=$altText")
        .body(body)
        .contentType("application/octet-stream")
        .response(asBody)
    )

  override def deleteCover(bookId: Int): Task[Unit] =
    transport.unit(request.delete(uri"$baseUri/api/covers/$bookId").response(asBody))

  override def uploadCoverStreaming(bookId: Int, body: ZStream[Any, Throwable, Byte], title: Option[String])
    : Task[BookCoverDto] =
    transport.json[BookCoverDto](
      request
        .post(uri"$baseUri/api/covers/$bookId/stream?title=$title")
        .streamBody(streams)(body)
        .contentType("application/octet-stream")
        .response(asBody)
    )
}
