package io.okapi.exampleclient.api.models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BookCoverDto(bookId: Int, coverTitle: String, altText: String)

object BookCoverDto {
  given JsonValueCodec[BookCoverDto] = JsonCodecMaker.make
}
