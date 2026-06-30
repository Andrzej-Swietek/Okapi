package io.okapi.core
package http

import zio.json.*

import sttp.model.StatusCode

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

  final case class ApiErrorResponse(code: Int, message: String)
  object ApiErrorResponse {
    given JsonCodec[ApiErrorResponse] = DeriveJsonCodec.gen
  }

  def toResponse(e: ApiError): ApiErrorResponse =
    ApiErrorResponse(e.status.code, e.message)
}
