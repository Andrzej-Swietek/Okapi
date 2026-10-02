package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class CreateBookRequest(title: String, author: String, genre: String, year: Int)

object CreateBookRequest {
  given JsonValueCodec[CreateBookRequest] = JsonCodecMaker.make
}
