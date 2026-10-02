package io.okapi.core
package http

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.model.StatusCode
import sttp.tapir.{ Codec, CodecFormat, Schema }

/** An error a controller method fails with: the response gets [[status]] and an [[ApiError.ApiErrorResponse]] body. */
sealed trait ApiError {
  def status: StatusCode
  def message: String
}

object ApiError {
  final case class BadRequest(message: String) extends ApiError {
    val status: StatusCode = StatusCode.BadRequest
  }
  final case class NotFound(message: String) extends ApiError {
    val status: StatusCode = StatusCode.NotFound
  }
  final case class Unauthorized(message: String) extends ApiError {
    val status: StatusCode = StatusCode.Unauthorized
  }
  final case class Internal(message: String) extends ApiError {
    val status: StatusCode = StatusCode.InternalServerError
  }
  final case class Forbidden(message: String) extends ApiError {
    val status: StatusCode = StatusCode.Forbidden
  }
  final case class Conflict(message: String) extends ApiError {
    val status: StatusCode = StatusCode.Conflict
  }
  final case class UnprocessableEntity(message: String) extends ApiError {
    val status: StatusCode = StatusCode.UnprocessableEntity
  }
  final case class TooManyRequests(message: String) extends ApiError {
    val status: StatusCode = StatusCode.TooManyRequests
  }
  final case class ServiceUnavailable(message: String) extends ApiError {
    val status: StatusCode = StatusCode.ServiceUnavailable
  }
  final case class Other(status: StatusCode, message: String) extends ApiError {
    require(status.isClientError || status.isServerError, s"ApiError.Other needs a 4xx or 5xx status, got $status")
  }

  /** The JSON error body: `{"code": 404, "message": "..."}`. */
  final case class ApiErrorResponse(code: Int, message: String)

  object ApiErrorResponse {
    given Schema[ApiErrorResponse] = Schema.derived

    given JsonValueCodec[ApiErrorResponse] = JsonCodecMaker.make

    /** The error body's codec, independent of the JSON library used for other bodies. */
    given Codec[String, ApiErrorResponse, CodecFormat.Json] = sttp.tapir.json.jsoniter.jsoniterCodec
  }

  /** The `ApiError` an error response stands for: the matching case for its status, [[Other]] for other 4xx / 5xx
    * statuses, [[Internal]] for any other status.
    */
  def of(status: StatusCode, message: String): ApiError = {
    status.code match {
      case 400 => BadRequest(message)
      case 401 => Unauthorized(message)
      case 403 => Forbidden(message)
      case 404 => NotFound(message)
      case 409 => Conflict(message)
      case 422 => UnprocessableEntity(message)
      case 429 => TooManyRequests(message)
      case 500 => Internal(message)
      case 503 => ServiceUnavailable(message)
      case _ if status.isClientError || status.isServerError => Other(status, message)
      case _ => Internal(s"Unexpected status ${status.code}: $message")
    }
  }

  def toResponse(e: ApiError): ApiErrorResponse =
    ApiErrorResponse(e.status.code, e.message)
}

/** Carries an [[ApiError]] through effects whose error channel is `Throwable` (see
  * [[io.okapi.core.OkapiEffect.fromMonadError]]).
  */
final case class ApiErrorException(error: ApiError) extends RuntimeException(error.message)
