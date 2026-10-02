package io.okapi.core.macros.codecs

import io.okapi.core.OkapiRuntime
import io.okapi.core.macros.model.ParamKind
import io.okapi.core.macros.parsing.MethodParsing
import scala.quoted.*
import sttp.tapir.{ Codec, CodecFormat, EndpointInput, EndpointIO, MultipartCodec }

/** Tapir inputs for the request side: path segments, annotated parameters and the request body. */
private[okapi] trait InputCodecs extends MethodParsing with CodecSupport {
  import q.reflect.*

  def fixedPathInput(segment: String): Expr[EndpointInput[Unit]] =
    '{ EndpointInput.FixedPath(${ Expr(segment) }, Codec.idPlain(), EndpointIO.Info.empty) }

  def paramInput(param: Param): Expr[EndpointInput[?]] = {
    param.inputType.asType match {
      case '[t] =>
        val name = Expr(param.name)
        def codec[Raw: Type](raw: String): Expr[Codec[Raw, t, CodecFormat.TextPlain]] = {
          summonOrAbort[Codec[Raw, t, CodecFormat.TextPlain]](
            s"Missing Codec[$raw, T, TextPlain] for ${param.kind.annotation.show}(\"${param.name}\") of type " +
              s"${param.inputType.show}. Provide a Tapir Codec (e.g. import sttp.tapir.generic.auto.*)."
          )
        }
        param.kind match {
          case ParamKind.Path =>
            val c = codec[String]("String")
            '{ EndpointInput.PathCapture(Some($name), $c, EndpointIO.Info.empty) }
          case ParamKind.Query =>
            val c = codec[List[String]]("List[String]")
            '{ EndpointInput.Query($name, None, $c, EndpointIO.Info.empty) }
          case ParamKind.Header =>
            val c = codec[List[String]]("List[String]")
            '{ EndpointIO.Header($name, $c, EndpointIO.Info.empty) }
          case ParamKind.Cookie =>
            val c = codec[Option[String]]("Option[String]")
            '{ EndpointInput.Cookie($name, $c, EndpointIO.Info.empty) }
          case ParamKind.BearerAuth =>
            val c = codec[List[String]]("List[String]")
            '{ sttp.tapir.TapirAuth.bearer[t]()(using $c) }
        }
    }
  }

  /** Extension point: a backend module overrides this to support extra body types (e.g. streams). */
  def customRequestBody(tpe: TypeRepr, mediaType: Option[String]): Option[Expr[EndpointInput[?]]] = None

  def requestBodyInput(tpe: TypeRepr, mediaType: Option[String]): Expr[EndpointInput[?]] =
    customRequestBody(tpe, mediaType).getOrElse(standardRequestBody(tpe, mediaType))

  private def standardRequestBody(tpe: TypeRepr, mediaType: Option[String]): Expr[EndpointInput[?]] = {
    if tpe <:< TypeRepr.of[Array[Byte]] then bytesBody(mediaType)
    else if tpe <:< TypeRepr.of[String] then stringBody(mediaType)
    else {
      tpe.asType match {
        case '[t] =>
          mediaType.filterNot(isJson) match {
            case None =>
              val codec = jsonCodec[t]("request body")
              '{ OkapiRuntime.jsonBody[t]($codec) }
            case Some("application/x-www-form-urlencoded") =>
              val codec = summonOrAbort[Codec[String, t, CodecFormat.XWwwFormUrlencoded]](
                s"Missing Codec[String, ${tpe.show}, CodecFormat.XWwwFormUrlencoded] for " +
                  "@Consumes(\"application/x-www-form-urlencoded\"). Import sttp.tapir.generic.auto.*"
              )
              '{ OkapiRuntime.formBodyInput[t]($codec) }
            case Some("multipart/form-data") =>
              val codec = summonOrAbort[MultipartCodec[t]](
                s"Missing MultipartCodec[${tpe.show}] for @Consumes(\"multipart/form-data\"). " +
                  "Import sttp.tapir.generic.auto.*"
              )
              '{ OkapiRuntime.multipartBodyInput[t]($codec) }
            case Some(other) =>
              abort(
                s"@Consumes(\"$other\") cannot decode a ${tpe.show} body. Use application/json, " +
                  "application/x-www-form-urlencoded or multipart/form-data, or take the body as String / Array[Byte]."
              )
          }
      }
    }
  }
}
