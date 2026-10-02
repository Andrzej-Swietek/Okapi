package io.okapi.exampleclient.api

import io.okapi.exampleclient.api.models.{ Book, CreateUserRequest, User, UserPreferences }

/** The operations of `Users`. */
trait UsersRoutes[F[_]] {

  /** List all users */
  def listUsers(xAdminToken: Option[String] = None): F[List[User]]

  /** Register a new user */
  def createUser(createUserRequest: CreateUserRequest): F[User]

  /** Get user by ID */
  def getUser(id: Int): F[User]

  /** Get user preferences */
  def getPreferences(id: Int): F[UserPreferences]

  /** Get book recommendations for user */
  def getRecommendations(id: Int, genre: Option[String] = None, limit: Option[Int] = None): F[List[Book]]
}
