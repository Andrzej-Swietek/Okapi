package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }

final case class CreateUserRequest(username: String, email: String)

object CreateUserRequest {
  given JsonValueCodec[CreateUserRequest] = JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))
}
