package io.okapi.core

import io.okapi.core.http.ApiError
import io.okapi.core.http.ApiError.ApiErrorResponse
import io.okapi.core.http.FileResponse
import sttp.tapir.*
import sttp.tapir.ztapir.*
import zio.*
import zio.stream.ZStream

object OkapiRuntime {

  def addInput[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R, J, IJ](
    endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R],
    input: EndpointInput[J],
  )(using sttp.tapir.typelevel.ParamConcat.Aux[INPUT, J, IJ]
  ): Endpoint[SECURITY_INPUT, IJ, ERROR_OUTPUT, OUTPUT, R] =
    endpoint.in(input)

  def addOutput[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R, J, OJ](
    endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R],
    output: EndpointOutput[J],
  )(using sttp.tapir.typelevel.ParamConcat.Aux[OUTPUT, J, OJ]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OJ, R] =
    endpoint.out(output)

  def addErrorOutput[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R, J, EJ](
    endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R],
    errorOutput: EndpointOutput[J],
  )(using sttp.tapir.typelevel.ParamConcat.Aux[ERROR_OUTPUT, J, EJ]
  ): Endpoint[SECURITY_INPUT, INPUT, EJ, OUTPUT, R] =
    endpoint.errorOut(errorOutput)

  def mapApiErrorEffect[R, A](
    effect: ZIO[R, ApiError, A],
  ): ZIO[R, (sttp.model.StatusCode, ApiErrorResponse), A] =
    effect.mapError(error => (error.status, ApiError.toResponse(error)))

  def serviceWithMappedZio[T: zio.Tag, A](
    f: T => ZIO[Any, (sttp.model.StatusCode, ApiErrorResponse), A],
  ): ZIO[T, (sttp.model.StatusCode, ApiErrorResponse), A] =
    ZIO.serviceWithZIO[T](f)

  def jsonBodyInput[T](
    codec: zio.json.JsonCodec[T],
    schema: Schema[T],
  ): EndpointIO.Body[String, T] = {
    val encoder = zio.json.JsonEncoder.fromCodec(codec)
    val decoder = zio.json.JsonDecoder.fromCodec(codec)
    sttp.tapir.json.zio.jsonBody[T](using encoder, decoder, schema)
  }

  def jsonBodyOutput[T](
    codec: zio.json.JsonCodec[T],
    schema: Schema[T],
  ): EndpointOutput[T] = {
    val encoder = zio.json.JsonEncoder.fromCodec(codec)
    val decoder = zio.json.JsonDecoder.fromCodec(codec)
    sttp.tapir.json.zio.jsonBody[T](using encoder, decoder, schema)
  }

  def formBodyInput[T](codec: Codec[String, T, CodecFormat.XWwwFormUrlencoded]): EndpointIO.Body[String, T] =
    sttp.tapir.formBody[T](using codec)

  def formBodyOutput[T](codec: Codec[String, T, CodecFormat.XWwwFormUrlencoded]): EndpointOutput[T] =
    sttp.tapir.formBody[T](using codec)

  def multipartBodyInput[T](
    codec: MultipartCodec[T],
  ): EndpointIO.Body[Seq[RawPart], T] =
    sttp.tapir.multipartBody[T](using codec)

  def xmlStringBody: EndpointIO.Body[String, String] =
    sttp.tapir.stringBodyAnyFormat(
      Codec.id[String, CodecFormat.Xml](CodecFormat.Xml(), Schema.string),
      java.nio.charset.StandardCharsets.UTF_8.nn,
    )

  def jsStringBody: EndpointIO.Body[String, String] =
    sttp.tapir.stringBodyAnyFormat(
      Codec.id[String, CodecFormat.TextJavascript](CodecFormat.TextJavascript(), Schema.string),
      java.nio.charset.StandardCharsets.UTF_8.nn,
    )

  def eventStreamBody: EndpointIO.Body[String, String] =
    sttp.tapir.stringBodyAnyFormat(
      Codec.id[String, CodecFormat.TextEventStream](CodecFormat.TextEventStream(), Schema.string),
      java.nio.charset.StandardCharsets.UTF_8.nn,
    )

  def attachServerLogic[T, I, E, O](
    endpoint: Endpoint[Unit, I, E, O, Any],
    logic: I => ZIO[T, E, O],
  ): ZServerEndpoint[T, sttp.capabilities.WebSockets] =
    endpoint.zServerLogic(logic).asInstanceOf[ZServerEndpoint[T, sttp.capabilities.WebSockets]]

  def attachWsServerLogic[T, I, E, In, Out](
    endpoint: Endpoint[Unit, I, E, ZStream[Any, Throwable, In] => ZStream[Any, Throwable, Out], sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets],
    logic: I => ZIO[T, E, ZStream[Any, Throwable, In] => ZStream[Any, Throwable, Out]],
  ): ZServerEndpoint[T, sttp.capabilities.WebSockets] =
    endpoint.zServerLogic(logic).asInstanceOf[ZServerEndpoint[T, sttp.capabilities.WebSockets]]

  private def textWsBody =
    sttp.tapir.ztapir.webSocketBody[String, CodecFormat.TextPlain, String, CodecFormat.TextPlain](sttp.capabilities.zio.ZioStreams)

  private def binaryWsBody =
    sttp.tapir.ztapir.webSocketBody[Array[Byte], CodecFormat.OctetStream, Array[Byte], CodecFormat.OctetStream](sttp.capabilities.zio.ZioStreams)

  def addTextWsOutput[S, I, E](
    endpoint: Endpoint[S, I, E, Unit, Any],
  ): Endpoint[S, I, E, WsPipe[String, String], sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets] =
    endpoint.out(textWsBody)
      .asInstanceOf[Endpoint[S, I, E, WsPipe[String, String], sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets]]

  def addBinaryWsOutput[S, I, E](
    endpoint: Endpoint[S, I, E, Unit, Any],
  ): Endpoint[S, I, E, WsPipe[Array[Byte], Array[Byte]], sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets] =
    endpoint.out(binaryWsBody)
      .asInstanceOf[Endpoint[S, I, E, WsPipe[Array[Byte], Array[Byte]], sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets]]

  def mapFileResponseApiError[R](
    effect: ZIO[R, ApiError, FileResponse],
  ): ZIO[R, (sttp.model.StatusCode, ApiErrorResponse), (Array[Byte], String)] =
    effect
      .map(fr => (fr.data, s"""attachment; filename="${fr.filename}""""))
      .mapError(e => (e.status, ApiError.toResponse(e)))

  def liftPure[A](value: A): ZIO[Any, Nothing, A] =
    ZIO.succeed(value)
}
