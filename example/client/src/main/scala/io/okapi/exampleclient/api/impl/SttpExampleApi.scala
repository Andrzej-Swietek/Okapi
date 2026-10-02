package io.okapi.exampleclient.api.impl

import zio.Task

import io.okapi.exampleclient.api.{ AdminRoutes, BooksRoutes, CoversRoutes, ExampleApi, ExploreRoutes, UsersRoutes }
import sttp.capabilities.zio.ZioStreams
import sttp.client4.StreamBackend
import sttp.model.{ Header, Uri }

/** The Okapi Example API client over an sttp backend; every request goes to `baseUri` with `headers`. */
final class SttpExampleApi(backend: StreamBackend[Task, ZioStreams], baseUri: Uri, headers: Seq[Header] = Nil)
  extends ExampleApi[Task] {
  private val transport = SttpTransport(backend, baseUri, headers)

  override val admin: AdminRoutes[Task] = SttpAdminRoutes(transport)

  override val books: BooksRoutes[Task] = SttpBooksRoutes(transport)

  override val covers: CoversRoutes[Task] = SttpCoversRoutes(transport)

  override val explore: ExploreRoutes[Task] = SttpExploreRoutes(transport)

  override val users: UsersRoutes[Task] = SttpUsersRoutes(transport)
}
