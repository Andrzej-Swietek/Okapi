
package io.okapi.exampleclient.api

object ExampleApi {

  import sttp.tapir._
  import sttp.tapir.model._
  import sttp.tapir.generic.auto._
  import sttp.tapir.json.jsoniter._
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import com.github.plokhotnyuk.jsoniter_scala.core._
  
  import io.okapi.exampleclient.api.ExampleApiJsonSerdes._
  import ExampleApiSchemas._


  
  
  
  
  case class CommaSeparatedValues[T](values: List[T])
  case class ExplodedValues[T](values: List[T])
  trait ExtraParamSupport[T] {
    def decode(s: String): sttp.tapir.DecodeResult[T]
    def encode(t: T): String
  }
  implicit def makePathCodecFromSupport[T](implicit support: ExtraParamSupport[T]): sttp.tapir.Codec[String, T, sttp.tapir.CodecFormat.TextPlain] = {
    sttp.tapir.Codec.string.mapDecode(support.decode)(support.encode)
  }
  implicit def makeQueryCodecFromSupport[T](implicit support: ExtraParamSupport[T]): sttp.tapir.Codec[List[String], T, sttp.tapir.CodecFormat.TextPlain] = {
    sttp.tapir.Codec.listHead[String, String, sttp.tapir.CodecFormat.TextPlain]
      .mapDecode(support.decode)(support.encode)
  }
  implicit def makeQueryOptCodecFromSupport[T](implicit support: ExtraParamSupport[T]): sttp.tapir.Codec[List[String], Option[T], sttp.tapir.CodecFormat.TextPlain] = {
    sttp.tapir.Codec.listHeadOption[String, String, sttp.tapir.CodecFormat.TextPlain]
      .mapDecode(maybeV => DecodeResult.sequence(maybeV.toSeq.map(support.decode)).map(_.headOption))(_.map(support.encode))
  }
  implicit def makeUnexplodedQuerySeqCodecFromListHead[T](implicit support: sttp.tapir.Codec[List[String], T, sttp.tapir.CodecFormat.TextPlain]): sttp.tapir.Codec[List[String], CommaSeparatedValues[T], sttp.tapir.CodecFormat.TextPlain] = {
    sttp.tapir.Codec.listHead[String, String, sttp.tapir.CodecFormat.TextPlain]
      .mapDecode(values => DecodeResult.sequence(values.split(',').toSeq.map(e => support.rawDecode(List(e)))).map(s => CommaSeparatedValues(s.toList)))(_.values.flatMap(support.encode).mkString(","))
  }
  implicit def makeUnexplodedQueryOptSeqCodecFromListHead[T](implicit support: sttp.tapir.Codec[List[String], T, sttp.tapir.CodecFormat.TextPlain]): sttp.tapir.Codec[List[String], Option[CommaSeparatedValues[T]], sttp.tapir.CodecFormat.TextPlain] = {
    sttp.tapir.Codec.listHeadOption[String, String, sttp.tapir.CodecFormat.TextPlain]
      .mapDecode{
        case None => DecodeResult.Value(None)
        case Some(values) => DecodeResult.sequence(values.split(',').toSeq.map(e => support.rawDecode(List(e)))).map(r => Some(CommaSeparatedValues(r.toList)))
      }(_.map(_.values.flatMap(support.encode).mkString(",")))
  }
  implicit def makeExplodedQuerySeqCodecFromListSeq[T](implicit support: sttp.tapir.Codec[List[String], List[T], sttp.tapir.CodecFormat.TextPlain]): sttp.tapir.Codec[List[String], ExplodedValues[T], sttp.tapir.CodecFormat.TextPlain] = {
    support.mapDecode(l => DecodeResult.Value(ExplodedValues(l)))(_.values)
  }
  implicit class RichBody[A, T](bod: EndpointIO.Body[A, T]) {
    def widenBody[TT >: T]: EndpointIO.Body[A, TT] = bod.map(_.asInstanceOf[TT])(_.asInstanceOf[T])
  }
  implicit class RichStreamBody[A, T, R](bod: sttp.tapir.StreamBodyIO[A, T, R]) {
    def widenBody[TT >: T]: sttp.tapir.StreamBodyIO[A, TT, R] = bod.map(_.asInstanceOf[TT])(_.asInstanceOf[T])
  }
  type ByteString <: Array[Byte]
  implicit def toByteString(ba: Array[Byte]): ByteString = ba.asInstanceOf[ByteString]

  
  
  
  case class ApiErrorResponse (
    code: Int,
    message: String
  )
  case class Book (
    id: Int,
    title: String,
    author: String,
    genre: String,
    year: Int
  )
  case class BookCoverDto (
    bookId: Int,
    coverTitle: String,
    altText: String
  )
  case class BookReview (
    bookId: Int,
    reviewer: String,
    rating: Int,
    comment: String
  )
  case class BookStats (
    total: Int,
    genres: Option[Seq[String]] = None
  )
  case class CreateBookRequest (
    title: String,
    author: String,
    genre: String,
    year: Int
  )
  case class CreateUserRequest (
    username: String,
    email: String
  )
  case class User (
    id: Int,
    username: String,
    email: String
  )
  case class UserPreferences (
    userId: Int,
    theme: String,
    language: String
  )




  lazy val getApiBooks =
    endpoint
      .name("getApiBooks")
      .get
      .in(("api" / "books"))
      .in(query[Option[String]]("genre"))
      .in(query[Option[Int]]("limit"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[List[Book]].description(""))
      .tags(List("Books"))
  
  lazy val postApiBooks =
    endpoint
      .name("postApiBooks")
      .post
      .in(("api" / "books"))
      .in(jsonBody[CreateBookRequest])
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[Book].description("").and(statusCode(sttp.model.StatusCode(201))))
      .tags(List("Books"))
  
  lazy val getApiBooksStats =
    endpoint
      .name("getApiBooksStats")
      .get
      .in(("api" / "books" / "stats"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[BookStats].description(""))
      .tags(List("Books"))
  
  lazy val getApiBooksId =
    endpoint
      .name("getApiBooksId")
      .get
      .in(("api" / "books" / path[Int]("id")))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[Book].description(""))
      .tags(List("Books"))
  
  lazy val putApiBooksId =
    endpoint
      .name("putApiBooksId")
      .put
      .in(("api" / "books" / path[Int]("id")))
      .in(jsonBody[CreateBookRequest])
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[Book].description(""))
      .tags(List("Books"))
  
  lazy val deleteApiBooksId =
    endpoint
      .name("deleteApiBooksId")
      .delete
      .in(("api" / "books" / path[Int]("id")))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(statusCode(sttp.model.StatusCode(204)).description(""))
      .tags(List("Books"))
  
  lazy val getApiBooksIdReviews =
    endpoint
      .name("getApiBooksIdReviews")
      .get
      .in(("api" / "books" / path[Int]("id") / "reviews"))
      .in(query[Option[Int]]("limit"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[List[BookReview]].description(""))
      .tags(List("Books"))
  
  lazy val getApiUsers =
    endpoint
      .name("getApiUsers")
      .get
      .in(("api" / "users"))
      .in(header[Option[String]]("X-Admin-Token"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[List[User]].description(""))
      .tags(List("Users"))
  
  lazy val postApiUsers =
    endpoint
      .name("postApiUsers")
      .post
      .in(("api" / "users"))
      .in(jsonBody[CreateUserRequest])
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[User].description("").and(statusCode(sttp.model.StatusCode(201))))
      .tags(List("Users"))
  
  lazy val getApiUsersId =
    endpoint
      .name("getApiUsersId")
      .get
      .in(("api" / "users" / path[Int]("id")))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[User].description(""))
      .tags(List("Users"))
  
  lazy val getApiUsersIdPreferences =
    endpoint
      .name("getApiUsersIdPreferences")
      .get
      .in(("api" / "users" / path[Int]("id") / "preferences"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[UserPreferences].description(""))
      .tags(List("Users"))
  
  lazy val getApiUsersIdRecommendations =
    endpoint
      .name("getApiUsersIdRecommendations")
      .get
      .in(("api" / "users" / path[Int]("id") / "recommendations"))
      .in(query[Option[String]]("genre"))
      .in(query[Option[Int]]("limit"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[List[Book]].description(""))
      .tags(List("Users"))
  
  lazy val getApiExploreGenres =
    endpoint
      .name("getApiExploreGenres")
      .get
      .in(("api" / "explore" / "genres"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(stringBody.description(""))
      .tags(List("Explore"))
  
  lazy val getApiExploreGenreYearPopular =
    endpoint
      .name("getApiExploreGenreYearPopular")
      .get
      .in(("api" / "explore" / path[String]("genre") / path[Int]("year") / "popular"))
      .in(query[Option[Int]]("limit"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(stringBody.description(""))
      .tags(List("Explore"))
  
  lazy val getApiCoversBookid =
    endpoint
      .name("getApiCoversBookid")
      .get
      .in(("api" / "covers" / path[Int]("bookId")))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(streamBody(sttp.capabilities.zio.ZioStreams)(Schema.binary[Array[Byte]], CodecFormat.OctetStream()).description("").toEndpointIO.and(header[String]("Content-Disposition")))
      .tags(List("Covers"))
  
  lazy val postApiCoversBookid =
    endpoint
      .name("postApiCoversBookid")
      .post
      .in(("api" / "covers" / path[Int]("bookId")))
      .in(query[Option[String]]("title"))
      .in(query[Option[String]]("altText"))
      .in(streamBody(sttp.capabilities.zio.ZioStreams)(Schema.binary[Array[Byte]], CodecFormat.OctetStream()))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[BookCoverDto].description("").and(statusCode(sttp.model.StatusCode(201))))
      .tags(List("Covers"))
  
  lazy val deleteApiCoversBookid =
    endpoint
      .name("deleteApiCoversBookid")
      .delete
      .in(("api" / "covers" / path[Int]("bookId")))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(statusCode(sttp.model.StatusCode(204)).description(""))
      .tags(List("Covers"))
  
  lazy val postApiCoversBookidStream =
    endpoint
      .name("postApiCoversBookidStream")
      .post
      .in(("api" / "covers" / path[Int]("bookId") / "stream"))
      .in(query[Option[String]]("title"))
      .in(streamBody(sttp.capabilities.zio.ZioStreams)(Schema.binary[Array[Byte]], CodecFormat.OctetStream()))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[BookCoverDto].description("").and(statusCode(sttp.model.StatusCode(201))))
      .tags(List("Covers"))
  
  lazy val getApiAdminExport =
    endpoint
      .name("getApiAdminExport")
      .get
      .in(("api" / "admin" / "export"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(streamBody(sttp.capabilities.zio.ZioStreams)(Schema.binary[Array[Byte]], CodecFormat.OctetStream()).description(""))
      .tags(List("Admin"))
  
  lazy val getApiAdminHealth =
    endpoint
      .name("getApiAdminHealth")
      .get
      .in(("api" / "admin" / "health"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(stringBody.description(""))
      .tags(List("Admin"))
  
  lazy val getApiAdminStats =
    endpoint
      .name("getApiAdminStats")
      .get
      .in(("api" / "admin" / "stats"))
      .errorOut(oneOf[ApiErrorResponse](
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(400), jsonBody[ApiErrorResponse].description("Bad request")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(404), jsonBody[ApiErrorResponse].description("Not found")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(429), jsonBody[ApiErrorResponse].description("Too many requests")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(500), jsonBody[ApiErrorResponse].description("Internal server error")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(403), jsonBody[ApiErrorResponse].description("Forbidden")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(422), jsonBody[ApiErrorResponse].description("Unprocessable entity")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(409), jsonBody[ApiErrorResponse].description("Conflict")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(503), jsonBody[ApiErrorResponse].description("Service unavailable")),
        oneOfVariant[ApiErrorResponse](sttp.model.StatusCode(401), jsonBody[ApiErrorResponse].description("Unauthorized"))))
      .out(jsonBody[BookStats].description(""))
      .tags(List("Admin"))
  
  
  lazy val generatedEndpoints = List(getApiBooks, postApiBooks, getApiBooksStats, getApiBooksId, putApiBooksId, deleteApiBooksId, getApiBooksIdReviews, getApiUsers, postApiUsers, getApiUsersId, getApiUsersIdPreferences, getApiUsersIdRecommendations, getApiExploreGenres, getApiExploreGenreYearPopular, getApiCoversBookid, postApiCoversBookid, deleteApiCoversBookid, postApiCoversBookidStream, getApiAdminExport, getApiAdminHealth, getApiAdminStats)

}
