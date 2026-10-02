package io.okapi.core
package http

/** A download: `data` is the body (with the [[io.okapi.core.annotations.Produces]] media type,
  * `application/octet-stream` by default) and the response gets `Content-Disposition: attachment; filename="..."`, with
  * quotes and CR/LF removed from `filename`.
  */
final case class FileResponse(
  data: Array[Byte],
  filename: String,
)
