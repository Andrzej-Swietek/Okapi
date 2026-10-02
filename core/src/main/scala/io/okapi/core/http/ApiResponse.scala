package io.okapi.core
package http

import sttp.model.{ Header, HeaderNames, MediaType, StatusCode }

/** A success result whose status and headers are chosen per call. A method returning `ApiResponse[A]` (or `F` of it)
  * documents `A` as its body; `status = None` sends the endpoint's success status (`@Status`, else 204 for `Unit`, 201
  * for POST, 200 otherwise).
  */
final case class ApiResponse[+A](body: A, status: Option[StatusCode] = None, headers: List[Header] = Nil) {

  def withStatus(status: StatusCode): ApiResponse[A] = copy(status = Some(status))

  def withHeader(name: String, value: String): ApiResponse[A] = copy(headers = headers :+ Header(name, value))

  /** Replaces the body's declared `Content-Type`. */
  def withContentType(mediaType: MediaType): ApiResponse[A] =
    copy(headers = headers.filterNot(_.is(HeaderNames.ContentType)) :+ Header.contentType(mediaType))
}
