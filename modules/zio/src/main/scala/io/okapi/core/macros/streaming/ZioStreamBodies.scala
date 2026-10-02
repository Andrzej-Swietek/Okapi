package io.okapi.core.macros.streaming

import io.okapi.core.{ OkapiRuntime, OkapiZioRuntime }
import io.okapi.core.macros.codecs.{ InputCodecs, OutputCodecs }
import scala.quoted.*
import sttp.model.sse.ServerSentEvent
import sttp.tapir.{ CodecFormat, EndpointInput, EndpointIO, EndpointOutput }

/** `ZStream` bodies: `ZStream[Any, Throwable, Byte]` in requests and responses (with the declared media type,
  * `application/octet-stream` by default), and `ZStream[Any, Throwable, ServerSentEvent]` responses as
  * `text/event-stream`.
  */
private[okapi] trait ZioStreamBodies extends InputCodecs with OutputCodecs {
  import q.reflect.*

  override def customRequestBody(tpe: TypeRepr, mediaType: Option[String]): Option[Expr[EndpointInput[?]]] = {
    if streamOf(tpe, TypeRepr.of[Byte]) then Some(byteStreamBody(mediaType))
    else super.customRequestBody(tpe, mediaType)
  }

  override def customResponseBody(tpe: TypeRepr, mediaType: Option[String]): Option[Expr[EndpointOutput[?]]] = {
    if streamOf(tpe, TypeRepr.of[Byte]) then Some(byteStreamBody(mediaType))
    else if streamOf(tpe, TypeRepr.of[ServerSentEvent]) then Some('{ OkapiZioRuntime.serverSentEventsBody })
    else super.customResponseBody(tpe, mediaType)
  }

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
