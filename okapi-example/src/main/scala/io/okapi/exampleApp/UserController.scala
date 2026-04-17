package io.okapi.exampleApp

import zio.{ IO, ZIO }
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/users")
@Tag("Users")
final class UserController(authService: AuthService, bookService: BookService) {

  @Get("")
  @Summary("List all users")
  def listUsers(
    @Header("X-Admin-Token") adminToken: Option[String],
  ): IO[ApiError, List[User]] =
    if adminToken.isEmpty then ZIO.fail(ApiError.Unauthorized("Missing X-Admin-Token header"))
    else authService.listUsers

  @Post("")
  @Summary("Register a new user")
  @Consumes("application/json")
  def createUser(
    @RequestBody body: CreateUserRequest,
  ): IO[ApiError, User] =
    authService.createUser(body)

  @Get("/{id}")
  @Summary("Get user by ID")
  def getUser(@Path("id") id: Int): IO[ApiError, User] =
    authService.getUser(id)

  @Get("/{id}/preferences")
  @Summary("Get user preferences")
  def getPreferences(@Path("id") id: Int): IO[ApiError, UserPreferences] =
    authService.getPreferences(id)

  @Get("/{id}/recommendations")
  @Summary("Get book recommendations for user")
  def getRecommendations(
    @Path("id") id: Int,
    @Query("genre") genre: Option[String],
    @Query("limit") limit: Option[Int],
  ): IO[ApiError, List[Book]] =
    authService.getUser(id).flatMap(_ => bookService.listBooks(genre, limit.getOrElse(5)))
}
