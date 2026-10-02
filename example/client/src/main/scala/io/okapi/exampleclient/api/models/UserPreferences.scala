package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class UserPreferences(userId: Int, theme: String, language: String)

object UserPreferences {
  given JsonValueCodec[UserPreferences] = JsonCodecMaker.make
}
