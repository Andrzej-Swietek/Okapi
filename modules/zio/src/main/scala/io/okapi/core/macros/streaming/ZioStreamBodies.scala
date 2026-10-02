package io.okapi.core.macros.streaming

import zio.stream.ZStream

import io.okapi.core.{ OkapiRuntime, OkapiZioRuntime }
import io.okapi.core.macros.codecs.{ InputCodecs, OutputCodecs }
import scala.quoted.*
import sttp.model.sse.ServerSentEvent
import sttp.tapir.{ CodecFormat, EndpointInput, EndpointIO, EndpointOutput }

/** `ZStream` bodies: `ZStream[Any, Throwable, Byte]` in requests and responses (with the declared media type,
  * `application/octet-stream` by default), and `ZStream[Any, Throwable, ServerSentEvent]` responses as
  * `text/event-stream`. A stream body of another environment or error type is a compile error.
  */
private[okapi] trait ZioStreamBodies extends InputCodecs with OutputCodecs {
  import q.reflect.*

  override def customRequestBody(tpe: TypeRepr, mediaType: Option[String]): Option[Expr[EndpointInput[?]]] = {
    if streamOf(tpe, TypeRepr.of[Byte]) then {
      if !(TypeRepr.of[ZStream[Any, Throwable, Byte]] <:< tpe) then {
        abort(
          s"A request body of type ${short(tpe)} is not supported: Okapi passes the body as a " +
            "ZStream[Any, Throwable, Byte], whose failures are not of the declared error type. Declare the parameter " +
            "as ZStream[Any, Throwable, Byte] and map its errors in the method."
        )
      }
      Some(byteStreamBody(mediaType))
    }
    else super.customRequestBody(tpe, mediaType)
  }

  override def customResponseBody(tpe: TypeRepr, mediaType: Option[String]): Option[Expr[EndpointOutput[?]]] = {
    if streamOf(tpe, TypeRepr.of[Byte]) then {
      requireServedStream(tpe, TypeRepr.of[Byte])
      Some(byteStreamBody(mediaType))
    }
    else if streamOf(tpe, TypeRepr.of[ServerSentEvent]) then {
      requireServedStream(tpe, TypeRepr.of[ServerSentEvent])
      Some('{ OkapiZioRuntime.serverSentEventsBody })
    }
    else super.customResponseBody(tpe, mediaType)
  }

  private def requireServedStream(tpe: TypeRepr, element: TypeRepr): Unit = {
    element.asType match {
      case '[e] =>
        if !(tpe <:< TypeRepr.of[ZStream[Any, Throwable, e]]) then {
          abort(
            s"A response body of type ${short(tpe)} is not supported: Okapi runs it as a " +
              s"ZStream[Any, Throwable, ${short(element)}], so a stream needing an environment fails when it runs and " +
              "a non-Throwable error breaks the response. Provide the stream's environment in the method (e.g. " +
              "`.provideEnvironment`) and map its errors to a Throwable (e.g. `.mapError(ApiErrorException(_))`)."
          )
        }
    }
  }

  private def short(tpe: TypeRepr): String = tpe.show(using Printer.TypeReprShortCode)

  private def streamOf(tpe: TypeRepr, element: TypeRepr): Boolean = {
    tpe.dealias match {
      case AppliedType(tc, List(_, _, elem)) if tc.typeSymbol.fullName == "zio.stream.ZStream" =>
        elem.dealias =:= element
      case _ => false
    }
  }

  private def byteStreamBody(mediaType: Option[String]): Expr[EndpointIO[?]] = {
    val format =
      mediaType.fold('{ CodecFormat.OctetStream(): CodecFormat })(m => '{ OkapiRuntime.MediaFormat(${ Expr(m) }) })
    '{
      EndpointIO.StreamBodyWrapper(
        sttp.tapir.streamBody(sttp.capabilities.zio.ZioStreams)(sttp.tapir.Schema.binary, $format)
      )
    }
  }
}
