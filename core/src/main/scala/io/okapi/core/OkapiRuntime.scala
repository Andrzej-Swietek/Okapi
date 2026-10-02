package io.okapi.core

import java.nio.charset.StandardCharsets

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import io.okapi.core.http.{ ApiResponse, FileResponse }
import io.okapi.core.http.ApiError
import io.okapi.core.http.ApiError.ApiErrorResponse
import sttp.model.{ Header, MediaType, StatusCode, StatusText }
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

/** Runtime helpers the generated endpoint code calls into. */
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

  def andInput[A, B, AB](
    left: EndpointInput[A],
    right: EndpointInput[B],
  )(using sttp.tapir.typelevel.ParamConcat.Aux[A, B, AB]
  ): EndpointInput[AB] =
    left.and(right)

  def andOutput[A, B, AB](
    left: EndpointOutput[A],
    right: EndpointOutput[B],
  )(using sttp.tapir.typelevel.ParamConcat.Aux[A, B, AB]
  ): EndpointOutput[AB] =
    left.and(right)

  /** A per-call status, documented as `defaultStatus`. */
  def apiResponseStatus(defaultStatus: Int): EndpointOutput.StatusCode[StatusCode] = {
    val code = StatusCode(defaultStatus)
    sttp.tapir.statusCode.description(code, StatusText.default(code).getOrElse(""))
  }

  /** A per-call status and headers before a body of type `A`; `defaultStatus` is sent when the status is not set. */
  def apiResponseOutput[A](
    output: EndpointOutput[(StatusCode, List[Header], A)],
    defaultStatus: Int,
  ): EndpointOutput[ApiResponse[A]] = {
    output.map((status, headers, body) => ApiResponse(body, Some(status), headers))(response =>
      (response.status.getOrElse(StatusCode(defaultStatus)), response.headers, response.body)
    )
  }

  /** [[apiResponseOutput]] for an empty body. */
  def apiResponseUnitOutput(output: EndpointOutput[(StatusCode, List[Header])], defaultStatus: Int)
    : EndpointOutput[ApiResponse[Unit]] = {
    output.map((status, headers) => ApiResponse((), Some(status), headers))(response =>
      (response.status.getOrElse(StatusCode(defaultStatus)), response.headers)
    )
  }

  /** The values of several request inputs, carried as a single input value. */
  final class InputGroup(val values: Any)

  def grouped[V](input: EndpointInput[V]): EndpointInput[InputGroup] =
    input.map(InputGroup(_))(_.values.asInstanceOf[V])

  /** The error status, documented with the status of every [[ApiError]] case except [[ApiError.Other]]. */
  val apiErrorStatus: EndpointOutput.StatusCode[StatusCode] = {
    List(
      StatusCode.BadRequest -> "Bad request",
      StatusCode.Unauthorized -> "Unauthorized",
      StatusCode.Forbidden -> "Forbidden",
      StatusCode.NotFound -> "Not found",
      StatusCode.Conflict -> "Conflict",
      StatusCode.UnprocessableEntity -> "Unprocessable entity",
      StatusCode.TooManyRequests -> "Too many requests",
      StatusCode.InternalServerError -> "Internal server error",
      StatusCode.ServiceUnavailable -> "Service unavailable",
    ).foldLeft(sttp.tapir.statusCode)((output, status) => output.description(status._1, status._2))
  }

  /** A JSON body from any Tapir JSON codec. */
  def jsonBody[T](codec: Codec[String, T, CodecFormat.Json]): EndpointIO.Body[String, T] =
    sttp.tapir.customCodecJsonBody[T](using codec)

  /** A JSON body served / read with exactly the given media type (UTF-8). */
  def jsonBody[T](codec: Codec[String, T, CodecFormat.Json], mediaType: String): EndpointIO.Body[String, T] =
    sttp.tapir.stringBodyAnyFormat(codec.format(MediaFormat(mediaType)), StandardCharsets.UTF_8.nn)

  /** A Tapir JSON codec from a jsoniter-scala codec: Okapi's default JSON support. */
  def jsoniterCodec[T](codec: JsonValueCodec[T], schema: Schema[T]): Codec[String, T, CodecFormat.Json] =
    sttp.tapir.json.jsoniter.jsoniterCodec[T](using codec, schema)

  def formBodyInput[T](codec: Codec[String, T, CodecFormat.XWwwFormUrlencoded]): EndpointIO.Body[String, T] =
    sttp.tapir.formBody[T](using codec)

  def formBodyOutput[T](codec: Codec[String, T, CodecFormat.XWwwFormUrlencoded]): EndpointOutput[T] =
    formBodyInput(codec)

  def multipartBodyInput[T](
    codec: MultipartCodec[T]
  ): EndpointIO.Body[Seq[RawPart], T] =
    sttp.tapir.multipartBody[T](using codec)

  /** A `String` body served / read with exactly the given media type (UTF-8). */
  def textBody(mediaType: String): EndpointIO.Body[String, String] =
    sttp.tapir.stringBodyAnyFormat(Codec.id(MediaFormat(mediaType), Schema.string), StandardCharsets.UTF_8.nn)

  /** An `Array[Byte]` body served / read with exactly the given media type. */
  def binaryBody(mediaType: String): EndpointIO.Body[Array[Byte], Array[Byte]] = {
    EndpointIO.Body(
      RawBodyType.ByteArrayBody,
      Codec.id(MediaFormat(mediaType), Schema.schemaForByteArray),
      EndpointIO.Info.empty,
    )
  }

  /** Codec format for a media type; the `String` overload throws on an unparsable media type. */
  final case class MediaFormat(mediaType: MediaType) extends CodecFormat

  object MediaFormat {
    def apply(mediaType: String): MediaFormat = MediaFormat(MediaType.unsafeParse(mediaType))
  }

  def attachServerLogic[G[_], I, E, O, EC, R](
    endpoint: Endpoint[Unit, I, E, O, EC],
    logic: I => G[Either[E, O]],
  ): ServerEndpoint[R, G] =
    endpoint.serverLogic[G](logic).asInstanceOf[ServerEndpoint[R, G]]

  type ErrorOut = (sttp.model.StatusCode, ApiErrorResponse)

  /** Runs a controller call through its host and maps an [[ApiError]] to [[ErrorOut]]. */
  def serve[C, F[_], G[_], A](host: ControllerHost[C, F, G], call: C => F[A]): G[Either[ErrorOut, A]] =
    host.monad.map(host.run(call))(_.left.map(toErrorOut))

  /** Like [[serve]], splitting a [[FileResponse]] into body bytes and a `Content-Disposition` header. */
  def serveFile[C, F[_], G[_]](
    host: ControllerHost[C, F, G],
    call: C => F[FileResponse],
  ): G[Either[ErrorOut, (Array[Byte], String)]] =
    host.monad.map(host.run(call))(_.left.map(toErrorOut).map(fileParts))

  def pure[C, F[_], G[_], A](host: ControllerHost[C, F, G], value: => A): F[A] =
    host.effect.pure(value)

  private def toErrorOut(error: ApiError): ErrorOut = (error.status, ApiError.toResponse(error))

  private def fileParts(file: FileResponse): (Array[Byte], String) =
    (file.data, contentDisposition(file.filename))

  private def contentDisposition(filename: String): String = {
    val ascii = filename.iterator.map(c => if c >= ' ' && c <= '~' && c != '"' && c != '\\' then c else '_').mkString
    val attachment = s"""attachment; filename="$ascii""""
    if filename.forall(_.toInt < 0x80) then attachment
    else s"$attachment; filename*=UTF-8''${percentEncode(filename)}"
  }

  /** RFC 8187 `value-chars`: UTF-8 bytes, each outside `attr-char` as `%XX`. */
  private def percentEncode(value: String): String = {
    value
      .getBytes(StandardCharsets.UTF_8)
      .nn
      .map { byte =>
        val c = (byte & 0xff).toChar
        val attrChar = c.isLetterOrDigit && c <= '~' || "!#$&+-.^_`|~".contains(c)
        if attrChar then c.toString else f"%%${byte & 0xff}%02X"
      }
      .mkString
  }
}
