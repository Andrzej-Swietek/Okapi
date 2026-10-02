package io.okapi.core
package http

/** A download: `data` is the body (with the [[io.okapi.core.annotations.Produces]] media type,
  * `application/octet-stream` by default) and the response gets `Content-Disposition: attachment; filename="..."`, with
  * each `"`, `\` and character outside printable ASCII in `filename` replaced by `_`; a `filename` with non-ASCII
  * characters is also sent exactly, as `filename*=UTF-8''<percent-encoded>`.
  */
final case class FileResponse(
  data: Array[Byte],
  filename: String,
)
