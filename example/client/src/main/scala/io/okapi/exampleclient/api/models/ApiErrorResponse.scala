package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ApiErrorResponse(code: Int, message: String)

object ApiErrorResponse {
  given JsonValueCodec[ApiErrorResponse] = JsonCodecMaker.make
}
