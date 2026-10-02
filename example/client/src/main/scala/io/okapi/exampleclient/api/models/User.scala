package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class User(id: Int, username: String, email: String)

object User {
  given JsonValueCodec[User] = JsonCodecMaker.make
}
