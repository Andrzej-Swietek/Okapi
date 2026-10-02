package io.okapi.exampleclient.api.impl

import zio.Task

import io.okapi.exampleclient.api.ExploreRoutes
import sttp.model.Uri.UriContext

/** The sttp implementation of [[ExploreRoutes]]. */
final class SttpExploreRoutes(transport: SttpTransport) extends ExploreRoutes[Task] {
  import transport.{ asBody, baseUri, request }

  override def genres(): Task[String] =
    transport.text(request.get(uri"$baseUri/api/explore/genres").response(asBody))

  override def popularByGenreYear(genre: String, year: Int, limit: Option[Int]): Task[String] =
    transport.text(request.get(uri"$baseUri/api/explore/$genre/$year/popular?limit=$limit").response(asBody))
}
