package io.okapi.exampleclient.api.impl

import zio.Task

import io.okapi.exampleclient.api.AdminRoutes
import io.okapi.exampleclient.api.models.BookStats
import sttp.model.Uri.UriContext

/** The sttp implementation of [[AdminRoutes]]. */
final class SttpAdminRoutes(transport: SttpTransport) extends AdminRoutes[Task] {
  import transport.{ asBody, baseUri, request }

  override def exportCsv(): Task[Array[Byte]] =
    transport.bytes(request.get(uri"$baseUri/api/admin/export").response(asBody))

  override def health(): Task[String] =
    transport.text(request.get(uri"$baseUri/api/admin/health").response(asBody))

  override def stats(): Task[BookStats] =
    transport.json[BookStats](request.get(uri"$baseUri/api/admin/stats").response(asBody))
}
