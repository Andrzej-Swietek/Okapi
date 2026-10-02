package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BookStats(total: Int, genres: List[String] = Nil)

object BookStats {
  given JsonValueCodec[BookStats] = JsonCodecMaker.make
}
