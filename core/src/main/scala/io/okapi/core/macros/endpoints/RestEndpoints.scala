package io.okapi.core.macros.endpoints

import io.okapi.core.OkapiRuntime
import io.okapi.core.http.{ ApiResponse, FileResponse }
import io.okapi.core.macros.model.{ HttpVerb, OkapiAnnotation }
import scala.quoted.*

/** `@Get` / `@Post` / `@Put` / `@Delete` / `@Patch` methods → request/response endpoints. */
private[okapi] trait RestEndpoints extends EndpointGeneration {
  import q.reflect.*

  object RestGenerator extends EndpointGenerator {

    val annotations: Set[OkapiAnnotation] = HttpVerb.values.map(_.annotation).toSet

    protected def baseEndpoint(route: Route): Term = {
      verb(route) match {
        case HttpVerb.Get => '{ sttp.tapir.endpoint.get }.asTerm
        case HttpVerb.Post => '{ sttp.tapir.endpoint.post }.asTerm
        case HttpVerb.Put => '{ sttp.tapir.endpoint.put }.asTerm
        case HttpVerb.Delete => '{ sttp.tapir.endpoint.delete }.asTerm
        case HttpVerb.Patch => '{ sttp.tapir.endpoint.patch }.asTerm
      }
    }

    protected def requestBody(spec: MethodSpec): Option[TypeRepr] = spec.body

    protected def addOutputs(endpoint: Term, route: Route, spec: MethodSpec): Term = {
      spec.output.asType match {
        case '[ApiResponse[body]] => endpoint.withOutput(apiResponseOutput[body](route, spec))
        case _ =>
          val withBody = endpoint.withOutput(responseBodyOutput(spec.output, spec.produces).asTerm)
          successStatus(route, spec.output) match {
            case 200 => withBody
            case code => withBody.withOutput(statusCodeOutput(code).asTerm)
          }
      }
    }

    /** `statusCode.and(headers).and(<body>)` mapped to `ApiResponse[B]`. */
    private def apiResponseOutput[B: Type](route: Route, spec: MethodSpec): Term = {
      val body = TypeRepr.of[B]
      if body =:= TypeRepr.of[FileResponse] then {
        abort(
          s"'${spec.symbol.name}': ApiResponse[FileResponse] is not supported; " +
            "return ApiResponse[Array[Byte]] with a Content-Disposition header"
        )
      }
      val code = Expr(successStatus(route, body))
      val status = code.asTerm
      val bodyOutput = responseBodyOutput(body, spec.produces).asTerm
      val statusAndHeaders = '{ OkapiRuntime.apiResponseStatus($code).and(sttp.tapir.headers) }.asTerm
      val combined = statusAndHeaders.andOutput(bodyOutput)
      tupleElements(transputValueType(bodyOutput.tpe)).size match {
        case 0 => callRuntime("apiResponseUnitOutput", Nil, List(combined, status))
        case 1 => callRuntime("apiResponseOutput", List(body), List(combined, status))
        case _ => abort(s"ApiResponse[${body.show}] in '${spec.symbol.name}': the body must be a single value")
      }
    }

    private def successStatus(route: Route, body: TypeRepr): Int = {
      route.method.intAnnotationArg(OkapiAnnotation.Status).getOrElse {
        if body =:= TypeRepr.of[Unit] then 204
        else if verb(route) == HttpVerb.Post then 201
        else 200
      }
    }

    private def verb(route: Route): HttpVerb = {
      HttpVerb
        .fromAnnotation(route.annotation)
        .getOrElse(abort(s"Unsupported HTTP method annotation: ${route.annotation.show}"))
    }
  }
}
