package io.okapi.core
package http

/** Wraps a binary response with a download filename.
  * Okapi maps this to `byteArrayBody` + `Content-Disposition: attachment; filename="..."`.
  */
final case class FileResponse(
  data: Array[Byte],
  filename: String,
)