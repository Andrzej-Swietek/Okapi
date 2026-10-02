package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }

final case class BookReview(bookId: Int, reviewer: String, rating: Int, comment: String)

object BookReview {
  given JsonValueCodec[BookReview] = JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))
}
