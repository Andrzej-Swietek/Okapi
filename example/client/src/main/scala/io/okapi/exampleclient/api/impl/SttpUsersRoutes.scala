package io.okapi.exampleclient.api.impl

import zio.Task

import com.github.plokhotnyuk.jsoniter_scala.core.{ writeToArray, JsonValueCodec }
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }
import io.okapi.exampleclient.api.UsersRoutes
import io.okapi.exampleclient.api.models.{ Book, CreateUserRequest, User, UserPreferences }
import sttp.model.{ Header, MediaType }
import sttp.model.Uri.UriContext

/** The sttp implementation of [[UsersRoutes]]. */
final class SttpUsersRoutes(transport: SttpTransport) extends UsersRoutes[Task] {
  import transport.{ asBody, baseUri, request }

  private given listUserCodec: JsonValueCodec[List[User]] =
    JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))
  private given listBookCodec: JsonValueCodec[List[Book]] =
    JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))

  override def listUsers(xAdminToken: Option[String]): Task[List[User]] =
    transport.json[List[User]](
      request
        .get(uri"$baseUri/api/users")
        .headers(xAdminToken.map(v => Header("X-Admin-Token", v)).toSeq*)
        .response(asBody)
    )

  override def createUser(createUserRequest: CreateUserRequest): Task[User] =
    transport.json[User](
      request
        .post(uri"$baseUri/api/users")
        .body(writeToArray(createUserRequest))
        .contentType(MediaType.ApplicationJson)
        .response(asBody)
    )

  override def getUser(id: Int): Task[User] =
    transport.json[User](request.get(uri"$baseUri/api/users/$id").response(asBody))

  override def getPreferences(id: Int): Task[UserPreferences] =
    transport.json[UserPreferences](request.get(uri"$baseUri/api/users/$id/preferences").response(asBody))

  override def getRecommendations(id: Int, genre: Option[String], limit: Option[Int]): Task[List[Book]] =
    transport.json[List[Book]](
      request.get(uri"$baseUri/api/users/$id/recommendations?genre=$genre&limit=$limit").response(asBody)
    )
}
