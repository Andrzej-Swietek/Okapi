package com.okapi.core

import com.okapi.core.http.ApiError
import com.okapi.core.http.ApiError.ApiErrorResponse

import sttp.tapir.*
import sttp.tapir.ztapir.*
import zio.*
import zio.http.{ Response, Routes }

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

  def attachServerLogic[T, I, E, O](
    endpoint: Endpoint[Unit, I, E, O, Any],
    logic: I => ZIO[T, E, O],
  ): ZServerEndpoint[T, sttp.capabilities.WebSockets] =
    endpoint.zServerLogic(logic).asInstanceOf[ZServerEndpoint[T, sttp.capabilities.WebSockets]]

  def liftPure[A](value: A): ZIO[Any, Nothing, A] =
    ZIO.succeed(value)
}
