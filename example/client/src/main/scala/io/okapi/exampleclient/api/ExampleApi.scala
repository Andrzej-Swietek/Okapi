package io.okapi.exampleclient.api

/** The Okapi Example API client. */
trait ExampleApi[F[_]] {

  /** The operations of `Admin`. */
  def admin: AdminRoutes[F]

  /** The operations of `Books`. */
  def books: BooksRoutes[F]

  /** The operations of `Covers`. */
  def covers: CoversRoutes[F]

  /** The operations of `Explore`. */
  def explore: ExploreRoutes[F]

  /** The operations of `Users`. */
  def users: UsersRoutes[F]
}
