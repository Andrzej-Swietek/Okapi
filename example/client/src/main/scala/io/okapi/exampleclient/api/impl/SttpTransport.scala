package io.okapi.exampleclient.api.impl

import zio.Task
import zio.stream.ZStream

import java.nio.charset.StandardCharsets.UTF_8

import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, JsonValueCodec }
import io.okapi.exampleclient.api.ApiException
import sttp.capabilities.Effect
import sttp.capabilities.zio.ZioStreams
import sttp.client4.{
  asByteArray,
  asStreamUnsafe,
  basicRequest,
  GenericRequest,
  PartialRequest,
  ResponseAs,
  StreamBackend,
  StreamResponseAs,
}
import sttp.model.{ Header, ResponseMetadata, Uri }

/** Sends the client's requests, each with `headers`; a non-2xx response fails the effect with [[ApiException]]. */
final class SttpTransport(backend: StreamBackend[Task, ZioStreams], val baseUri: Uri, headers: Seq[Header]) {

  /** A request the client sends. */
  type Sendable[A] = GenericRequest[Either[ApiException, A], ZioStreams & Effect[Task]]

  /** A request with the client's headers. */
  val request: PartialRequest[Either[String, String]] = basicRequest.headers(headers*)

  /** The body of a 2xx response, else the [[ApiException]] the response is. */
  val asBody: ResponseAs[Either[ApiException, Array[Byte]]] = asByteArray.mapWithMetadata(failure)

  val streams: ZioStreams = ZioStreams

  /** The body of a 2xx response as a stream the caller must consume, else the [[ApiException]] it is. */
  val asStream: StreamResponseAs[Either[ApiException, ZStream[Any, Throwable, Byte]], ZioStreams] =
    asStreamUnsafe(streams).mapWithMetadata(failure)

  def json[A: JsonValueCodec](request: Sendable[Array[Byte]]): Task[A] = read(request)(readFromArray[A](_))

  def text(request: Sendable[Array[Byte]]): Task[String] = read(request)(new String(_, UTF_8))

  def bytes(request: Sendable[Array[Byte]]): Task[Array[Byte]] = read(request)(identity)

  def unit(request: Sendable[Array[Byte]]): Task[Unit] = read(request)(_ => ())

  def send[A](request: Sendable[A]): Task[A] = read(request)(identity)

  private def read[A, B](request: Sendable[A])(f: A => B): Task[B] = {
    val monad = backend.monad
    monad.flatMap(backend.send(request))(response => response.body.fold(monad.error, a => monad.eval(f(a))))
  }

  private def failure[A](body: Either[String, A], meta: ResponseMetadata): Either[ApiException, A] =
    body.left.map(ApiException(meta.code.code, _))
}
