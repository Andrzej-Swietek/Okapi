package io.okapi.exampleclient.api

/** The operations of `Explore`. */
trait ExploreRoutes[F[_]] {

  /** List all available genres (plain text) */
  def genres(): F[String]

  /** Popular books by genre and year (plain text) */
  def popularByGenreYear(genre: String, year: Int, limit: Option[Int] = None): F[String]
}
