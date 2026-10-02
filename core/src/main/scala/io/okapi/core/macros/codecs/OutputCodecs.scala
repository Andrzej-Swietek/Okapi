package io.okapi.core.macros.codecs

import io.okapi.core.OkapiRuntime
import io.okapi.core.http.ApiError.ApiErrorResponse
import io.okapi.core.http.FileResponse
import scala.quoted.*
import sttp.tapir.{ Codec, CodecFormat, EndpointOutput }

/** Tapir outputs for the response side: success body, fixed status and the shared `ApiError` output. */
private[okapi] trait OutputCodecs extends CodecSupport {
  import q.reflect.*

  /** Extension point: a backend module overrides this to support extra body types (e.g. streams). */
  def customResponseBody(tpe: TypeRepr, mediaType: Option[String]): Option[Expr[EndpointOutput[?]]] = None

  def responseBodyOutput(tpe: TypeRepr, mediaType: Option[String]): Expr[EndpointOutput[?]] =
    customResponseBody(tpe, mediaType).getOrElse(standardResponseBody(tpe, mediaType))

  private def standardResponseBody(tpe: TypeRepr, mediaType: Option[String]): Expr[EndpointOutput[?]] = {
    if tpe =:= TypeRepr.of[Unit] then '{ sttp.tapir.emptyOutput }
    else if tpe =:= TypeRepr.of[FileResponse] then
      '{ ${ bytesBody(mediaType) }.and(sttp.tapir.header[String]("Content-Disposition")) }
    else if tpe <:< TypeRepr.of[Array[Byte]] then bytesBody(mediaType)
    else if tpe <:< TypeRepr.of[String] then stringBody(mediaType)
    else typedBodyOutput(tpe, mediaType)
  }

  /** Body of a non-primitive type `T`: JSON (the default) or a form. */
  private def typedBodyOutput(tpe: TypeRepr, mediaType: Option[String]): Expr[EndpointOutput[?]] = {
    tpe.asType match {
      case '[t] =>
        mediaType.filterNot(isJson) match {
          case None =>
            val codec = jsonCodec[t]("response body")
            '{ OkapiRuntime.jsonBody[t]($codec) }
          case Some("application/x-www-form-urlencoded") =>
            val codec = summonOrAbort[Codec[String, t, CodecFormat.XWwwFormUrlencoded]](
              s"Missing Codec[String, ${tpe.show}, CodecFormat.XWwwFormUrlencoded] for " +
                "@Produces(\"application/x-www-form-urlencoded\")"
            )
            '{ OkapiRuntime.formBodyOutput[t]($codec) }
          case Some(other) =>
            abort(
              s"@Produces(\"$other\") cannot encode a ${tpe.show} result. Return a String (or Array[Byte] for " +
                "binary media types), or produce application/json / application/x-www-form-urlencoded."
            )
        }
    }
  }

  def statusCodeOutput(code: Int): Expr[EndpointOutput[Unit]] =
    '{ sttp.tapir.statusCode(sttp.model.StatusCode(${ Expr(code) })) }

  /** `(StatusCode, ApiErrorResponse)` — the error output shared by every endpoint. */
  def apiErrorOutput: Expr[EndpointOutput[(sttp.model.StatusCode, ApiErrorResponse)]] = {
    '{
      OkapiRuntime.apiErrorStatus.and(OkapiRuntime.jsonBody(summon[Codec[String, ApiErrorResponse, CodecFormat.Json]]))
    }
  }
}
