package io.okapi.exampleclient.api


object ExampleApiSchemas {
  import io.okapi.exampleclient.api.ExampleApi._
  import sttp.tapir.generic.auto._
  implicit lazy val byteStringSchema: sttp.tapir.Schema[ByteString] = sttp.tapir.Schema.schemaForByteArray.map(ba => Some(toByteString(ba)))(bs => bs)
  implicit lazy val apiErrorResponseTapirSchema: sttp.tapir.Schema[ApiErrorResponse] = sttp.tapir.Schema.derived
  implicit lazy val bookTapirSchema: sttp.tapir.Schema[Book] = sttp.tapir.Schema.derived
  implicit lazy val bookCoverDtoTapirSchema: sttp.tapir.Schema[BookCoverDto] = sttp.tapir.Schema.derived
  implicit lazy val bookReviewTapirSchema: sttp.tapir.Schema[BookReview] = sttp.tapir.Schema.derived
  implicit lazy val bookStatsTapirSchema: sttp.tapir.Schema[BookStats] = sttp.tapir.Schema.derived
  implicit lazy val createBookRequestTapirSchema: sttp.tapir.Schema[CreateBookRequest] = sttp.tapir.Schema.derived
  implicit lazy val createUserRequestTapirSchema: sttp.tapir.Schema[CreateUserRequest] = sttp.tapir.Schema.derived
  implicit lazy val userTapirSchema: sttp.tapir.Schema[User] = sttp.tapir.Schema.derived
  implicit lazy val userPreferencesTapirSchema: sttp.tapir.Schema[UserPreferences] = sttp.tapir.Schema.derived
}