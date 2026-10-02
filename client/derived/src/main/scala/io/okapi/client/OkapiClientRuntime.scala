package io.okapi.client

import java.lang.reflect.{ InvocationHandler, Method, Proxy }

import io.okapi.core.OkapiEffect
import io.okapi.core.http.{ ApiError, FileResponse }
import io.okapi.core.http.ApiError.ApiErrorResponse
import sttp.client4.Backend
import sttp.model.{ StatusCode, Uri }
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp4.SttpClientInterpreter

/** Runtime helpers the generated clients call into. */
object OkapiClientRuntime {

  /** One API method: its arguments in declaration order → its result. */
  type Call = Array[AnyRef | Null] => Any

  /** Sends `input` to `endpoint`; an error response fails `F` with the [[ApiError]] for its status, and a response that
    * cannot be decoded fails `F` with an exception.
    */
  def call[F[_], I, O](
    endpoint: Endpoint[Unit, I, (StatusCode, ApiErrorResponse), O, Any],
    baseUri: Uri,
    backend: Backend[F],
    effect: OkapiEffect[F],
    input: I,
  ): F[O] = {
    val send = SttpClientInterpreter().toClientThrowDecodeFailures(endpoint, Some(baseUri), backend)
    effect.monad.flatMap(send(input)) {
      case Right(output) => effect.monad.unit(output)
      case Left((status, error)) => effect.fail(ApiError.of(status, error.message))
    }
  }

  /** A [[FileResponse]] from the body bytes and the `Content-Disposition` header. */
  def fileResponse(parts: (Array[Byte], String)): FileResponse = {
    val (data, disposition) = parts
    FileResponse(data, """filename="([^"]*)"""".r.findFirstMatchIn(disposition).map(_.group(1).nn).getOrElse(""))
  }

  /** A routed method by its JVM name and parameter classes, and its call. */
  final case class Route(name: String, parameterTypes: List[Class[?]], call: Call)

  private type Signature = (String, List[Class[?] | Null])

  /** An instance of the API trait `api` dispatching each routed method to its call: by name and parameter classes, else
    * by name and parameter count when one route has them. Methods with an implementation (e.g. default-argument
    * getters) run it, `hashCode` and `equals` are by identity, and any other method throws
    * `UnsupportedOperationException`.
    */
  def proxy[A](api: Class[A], routes: List[Route]): A = {
    val bySignature: Map[Signature, Call] = routes.map(r => ((r.name, r.parameterTypes): Signature) -> r.call).toMap
    val byArity: Map[(String, Int), Call] =
      routes.groupBy(r => (r.name, r.parameterTypes.size)).collect { case (key, List(only)) => key -> only.call }
    val handler = new InvocationHandler {
      def invoke(self: Object, method: Method, args: Array[Object | Null] | Null): Object | Null = {
        val arguments: Array[AnyRef | Null] =
          if args == null then Array.empty else args.asInstanceOf[Array[AnyRef | Null]]
        val name = method.getName.nn
        val routed =
          bySignature.get((name, method.getParameterTypes.nn.toList)).orElse(byArity.get((name, arguments.length)))
        routed match {
          case Some(call) => call(arguments).asInstanceOf[Object]
          case None if method.getName == "toString" && arguments.isEmpty => s"OkapiClient[${api.getName}]"
          case None if method.getName == "hashCode" && arguments.isEmpty => Int.box(System.identityHashCode(self))
          case None if method.getName == "equals" && arguments.length == 1 => Boolean.box(self eq arguments(0))
          case None if method.isDefault => InvocationHandler.invokeDefault(self, method, arguments*)
          case None =>
            throw new UnsupportedOperationException(
              s"${method.getName} of ${api.getName} is not an Okapi-routed method, so the client cannot call it. " +
                "Annotate it with an HTTP method annotation, or give it an implementation."
            )
        }
      }
    }
    Proxy.newProxyInstance(api.getClassLoader, Array(api), handler).asInstanceOf[A]
  }
}
