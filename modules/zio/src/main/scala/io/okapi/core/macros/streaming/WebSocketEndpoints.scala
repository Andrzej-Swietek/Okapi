package io.okapi.core.macros.streaming

import zio.stream.ZStream

import io.okapi.core.WsPipe
import io.okapi.core.macros.endpoints.EndpointGeneration
import io.okapi.core.macros.model.OkapiAnnotation
import scala.quoted.*
import sttp.tapir.{ Codec, CodecFormat }
import sttp.ws.WebSocketFrame

/** `@WebSocket` methods returning `WsPipe[In, Out]` → WebSocket endpoints. */
private[okapi] trait WebSocketEndpoints extends EndpointGeneration {
  import q.reflect.*

  object WebSocketGenerator extends EndpointGenerator {

    val annotations: Set[OkapiAnnotation] = Set(OkapiAnnotation.WebSocket)

    protected def baseEndpoint(route: Route): Term = '{ sttp.tapir.endpoint.get }.asTerm

    protected def requestBody(spec: MethodSpec): Option[TypeRepr] = None

    protected def addOutputs(endpoint: Term, route: Route, spec: MethodSpec): Term = {
      val types = endpointBaseTypes(endpoint)
      val (in, out) = messageTypes(spec)
      val ping = route.method.findAnnotation(OkapiAnnotation.WebSocket).flatMap(intParam(_, "pingIntervalSeconds"))
      callModule(
        "io.okapi.core.OkapiZioRuntime",
        "addWsOutput",
        List(types.securityInput, types.input, types.error, in, out),
        List(
          endpoint,
          frameCodec(in, spec).asTerm,
          frameCodec(out, spec).asTerm,
          Literal(IntConstant(ping.getOrElse(13))),
        ),
      )
    }

    /** `String` → text frames, `Array[Byte]` → binary frames, any other type → text frames carrying its JSON. */
    private def frameCodec(message: TypeRepr, spec: MethodSpec): Expr[Codec[WebSocketFrame, ?, CodecFormat]] = {
      message.asType match {
        case '[String] => '{ Codec.textWebSocketFrame(using Codec.string) }
        case '[Array[Byte]] => '{ Codec.binaryWebSocketFrame(using Codec.byteArray) }
        case '[m] =>
          val json = jsonCodec[m](s"WebSocket messages of '${spec.symbol.name}'")
          '{ Codec.textWebSocketFrame(using $json) }
      }
    }

    /** `(In, Out)` of a method returning `WsPipe[In, Out]`; any other result type is a compile error. */
    private def messageTypes(spec: MethodSpec): (TypeRepr, TypeRepr) = {
      val zstream = TypeRepr.of[ZStream[Any, Throwable, Any]].typeSymbol
      spec.output.dealias match {
        case AppliedType(fn, List(inStream, outStream)) if fn.typeSymbol == defn.FunctionClass(1) =>
          (inStream.dealias, outStream.dealias) match {
            case (AppliedType(zsi, List(_, _, in)), AppliedType(zso, List(_, _, out)))
                 if zsi.typeSymbol == zstream && zso.typeSymbol == zstream =>
              requirePipe(spec, in, out)
              (in, out)
            case _ => notAPipe(spec)
          }
        case _ => notAPipe(spec)
      }
    }

    private def requirePipe(spec: MethodSpec, in: TypeRepr, out: TypeRepr): Unit = {
      (in.asType, out.asType) match {
        case ('[i], '[o]) =>
          if !(spec.output <:< TypeRepr.of[WsPipe[i, o]]) then {
            abort(
              s"@WebSocket method '${spec.symbol.name}' returns ${short(spec.output)}, which is not a " +
                s"WsPipe[${short(in)}, ${short(out)}]: Okapi runs the pipe on a ZStream[Any, Throwable, ${short(in)}] and " +
                s"expects a ZStream[Any, Throwable, ${short(out)}], so a stream needing an environment fails when it " +
                "runs and a non-Throwable error breaks the connection. Provide the stream's environment in the method " +
                "(e.g. `.provideEnvironment`) and map its errors to a Throwable (e.g. `.mapError(ApiErrorException(_))`)."
            )
          }
      }
    }

    private def short(tpe: TypeRepr): String = tpe.show(using Printer.TypeReprShortCode)

    private def notAPipe(spec: MethodSpec): Nothing = {
      abort(
        s"@WebSocket method '${spec.symbol.name}' must return WsPipe[In, Out] or a ZIO effect of it, " +
          "e.g. IO[ApiError, WsPipe[In, Out]]. " +
          s"Got: ${spec.output.show}"
      )
    }
  }
}
