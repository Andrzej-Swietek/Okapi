package io.okapi.core.macros.endpoints

import io.okapi.core.macros.codecs.OutputCodecs
import io.okapi.core.macros.model.{ EndpointDocs, OkapiAnnotation, RoutePath }

/** Template Method for turning one routed controller method into a `ServerEndpoint[R, G]`.
  *
  * The skeleton (request inputs → outputs → error output → docs → server logic) is fixed in
  * [[EndpointGenerator.generate]]; concrete generators only fill in the steps that differ.
  */
private[okapi] trait EndpointGeneration extends RequestInputs with OutputCodecs with ServerLogic {
  import q.reflect.*

  /** A controller method together with the routing annotation that mapped it.
    *
    * @param specificity
    *   sort key, most specific first (see [[RoutePath.specificityKey]]).
    */
  final case class Route(
    method: Symbol,
    annotation: OkapiAnnotation,
    path: RoutePath,
    specificity: String,
    docs: EndpointDocs,
  )

  abstract class EndpointGenerator {

    /** Names of the method annotations this generator handles. */
    def annotations: Set[OkapiAnnotation]

    /** Returns the `ServerEndpoint[target.capabilities, target.server]` term. */
    final def generate(target: Target, route: Route): Term = {
      val spec = parseMethod(target.controller, target.effect, route.method)
      val body = requestBody(spec).map(bodyInput(_, spec.consumes))
      val inputs = layout(requestInputs(route.path, spec) ++ body)
      val endpoint = addOutputs(applyInputs(baseEndpoint(route), inputs), route, spec)
        .withErrorOutput(apiErrorOutput.asTerm)
        .withDocs(route.docs)
      attach(target, endpoint, serverLogic(target, spec, inputs))
    }

    protected def baseEndpoint(route: Route): Term

    /** Request body type passed to the controller, if this kind of endpoint takes one. */
    protected def requestBody(spec: MethodSpec): Option[TypeRepr]

    protected def addOutputs(endpoint: Term, route: Route, spec: MethodSpec): Term

    private def attach(target: Target, endpoint: Term, logic: Term): Term = {
      val types = endpointBaseTypes(endpoint)
      callRuntime(
        "attachServerLogic",
        List(target.server, types.input, types.error, types.output, types.capabilities, target.capabilities),
        List(endpoint, logic),
      )
    }
  }
}
