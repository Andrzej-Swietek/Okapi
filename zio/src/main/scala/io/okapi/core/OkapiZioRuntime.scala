package io.okapi.core

import zio.http.{ Response, Routes }
import zio.stream.ZStream

import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.*
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import sttp.tapir.ztapir.{ ZioServerSentEvents, ZServerEndpoint }
import sttp.ws.WebSocketFrame

/** ZIO-only runtime helpers: interpreting endpoints as ZIO HTTP routes, and the WebSocket outputs generated endpoints
  * call into.
  */
object OkapiZioRuntime {

  /** A Tapir JSON codec from a zio-json codec: okapi-zio's default JSON support. */
  def zioJsonCodec[T](codec: zio.json.JsonCodec[T], schema: Schema[T]): Codec[String, T, CodecFormat.Json] =
    sttp.tapir.json.zio.zioCodec[T](using codec.encoder, codec.decoder, schema)

  /** ZIO HTTP routes needing exactly the environment `R` the endpoints run in, interpreted with `options` (Tapir
    * interceptors: CORS, metrics, logging, error handling, ...).
    */
  def toRoutes[R](
    endpoints: List[ZServerEndpoint[R, WebSockets]],
    options: ZioHttpServerOptions[Any],
  ): Routes[R, Response] = {
    // options needing nothing from the environment run as well in RIO[R, *]; ZioStreams is provided by the interpreter
    ZioHttpInterpreter(options.asInstanceOf[ZioHttpServerOptions[R]])
      .toHttp(endpoints.asInstanceOf[List[ZServerEndpoint[R, ZioStreams & WebSockets]]])
  }

  /** A `text/event-stream` body of server-sent events. */
  val serverSentEventsBody
    : EndpointIO.StreamBodyWrapper[ZioStreams.BinaryStream, ZStream[Any, Throwable, ServerSentEvent]] = {
    EndpointIO.StreamBodyWrapper(
      sttp
        .tapir
        .streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8.nn))
        .map(ZioServerSentEvents.parseBytesToSSE)(ZioServerSentEvents.serialiseSSEToBytes)
    )
  }

  /** Adds a WebSocket body whose frames are decoded with `requests` and encoded with `responses`; the server pings
    * every `pingIntervalSeconds` (0 disables pinging).
    */
  def addWsOutput[S, I, E, In, Out](
    endpoint: Endpoint[S, I, E, Unit, Any],
    requests: Codec[WebSocketFrame, In, CodecFormat],
    responses: Codec[WebSocketFrame, Out, CodecFormat],
    pingIntervalSeconds: Int,
  ): Endpoint[S, I, E, WsPipe[In, Out], ZioStreams & WebSockets] = {
    val body = sttp
      .tapir
      .ztapir
      .webSocketBody[In, CodecFormat, Out, CodecFormat](ZioStreams)(using requests, responses)
      .autoPing(Option.when(pingIntervalSeconds > 0)((pingIntervalSeconds.seconds, WebSocketFrame.ping)))
    endpoint.out(body).asInstanceOf[Endpoint[S, I, E, WsPipe[In, Out], ZioStreams & WebSockets]]
  }
}
