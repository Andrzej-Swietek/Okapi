package io.okapi.exampleclient.api

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import io.okapi.exampleclient.api.models.ApiErrorResponse
import scala.util.Try

/** A response with a non-2xx `status`. */
final case class ApiException(status: Int, body: String) extends RuntimeException(s"HTTP $status: $body") {

  /** The body as an [[ApiErrorResponse]], when it is one. */
  def error: Option[ApiErrorResponse] = Try(readFromString[ApiErrorResponse](body)).toOption
}
