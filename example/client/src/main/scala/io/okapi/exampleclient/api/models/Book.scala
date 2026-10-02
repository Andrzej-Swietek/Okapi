package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }

final case class Book(id: Int, title: String, author: String, genre: String, year: Int)

object Book {
  given JsonValueCodec[Book] = JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))
}
