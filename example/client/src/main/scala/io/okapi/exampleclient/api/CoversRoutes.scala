package io.okapi.exampleclient.api

import zio.stream.ZStream

import io.okapi.exampleclient.api.models.BookCoverDto

/** The operations of `Covers`. */
trait CoversRoutes[F[_]] {

  /** Download book cover with Content-Disposition header */
  def downloadCover(bookId: Int): F[Array[Byte]]

  /** Upload a book cover image (raw binary, title/altText as query params) */
  def uploadCover(bookId: Int, body: Array[Byte], title: Option[String] = None, altText: Option[String] = None)
    : F[BookCoverDto]

  /** Delete book cover */
  def deleteCover(bookId: Int): F[Unit]

  /** Upload a book cover image (streaming binary body) */
  def uploadCoverStreaming(bookId: Int, body: ZStream[Any, Throwable, Byte], title: Option[String] = None)
    : F[BookCoverDto]
}
