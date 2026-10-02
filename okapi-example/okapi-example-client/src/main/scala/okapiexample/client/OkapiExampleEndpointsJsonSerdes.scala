package okapiexample.client

object OkapiExampleEndpointsJsonSerdes {
  import okapiexample.client.OkapiExampleEndpoints._
  import sttp.tapir.generic.auto._
  implicit val byteStringJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ByteString] = new com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ByteString] {
    def nullValue: ByteString = Array.empty[Byte]
    def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: ByteString): ByteString =
      toByteString(java.util.Base64.getDecoder.decode(in.readString("")))
    def encodeValue(x: ByteString, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): _root_.scala.Unit =
      out.writeVal(java.util.Base64.getEncoder.encodeToString(x))
  }
  
  implicit def seqCodec[T: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec]: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[List[T]] =
    com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make[List[T]]
  implicit def optionCodec[T: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec]: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[Option[T]] =
    com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make[Option[T]]
  
  implicit lazy val apiErrorResponseJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[ApiErrorResponse] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val bookCoverDtoJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[BookCoverDto] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val bookJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[Book] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val bookReviewJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[BookReview] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val bookStatsJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[BookStats] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val createBookRequestJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[CreateBookRequest] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val createUserRequestJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[CreateUserRequest] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val userJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[User] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
  implicit lazy val userPreferencesJsonCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[UserPreferences] = com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker.make(com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig.withAllowRecursiveTypes(true).withTransientEmpty(false).withTransientDefault(false).withRequireCollectionFields(true))
}