package io.okapi.core.macros

import io.okapi.core.ControllerHost
import io.okapi.core.macros.endpoints.RestEndpoints
import io.okapi.core.macros.model.{ EndpointDocs, OkapiAnnotation, RoutePath }
import scala.quoted.*
import sttp.tapir.server.ServerEndpoint

/** Expansion of `endpoints[C, F, G, R](host)`: one endpoint per controller method carrying a routing annotation of one
  * of the [[generators]], ordered most specific path first, then by path.
  */
private[okapi] class EndpointsMacro(val q: Quotes) extends RestEndpoints {
  import q.reflect.*

  protected def generators: List[EndpointGenerator] = List(RestGenerator)

  /** Controller methods some generator turns into an endpoint. */
  final def routedMethods(controller: TypeRepr): List[Symbol] =
    controller.typeSymbol.methodMembers.filter(routing(_).isDefined)

  final def expand[C: Type, F[_]: Type, G[_]: Type, R: Type](
    host: Expr[ControllerHost[C, F, G]]
  ): Expr[List[ServerEndpoint[R, G]]] = {
    val target = Target(TypeRepr.of[C], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[R], host.asTerm)
    val controller = target.controller.typeSymbol
    val basePath = controller.annotationArg(OkapiAnnotation.Controller).getOrElse("")
    val tag = controller
      .nonEmptyAnnotationArg(OkapiAnnotation.ApiTag)
      .orElse(controller.nonEmptyAnnotationArg(OkapiAnnotation.Tag))
      .getOrElse(controller.name)

    def route(method: Symbol, kind: OkapiAnnotation, annotation: Term): Route = {
      val path = RoutePath.join(basePath, stringArg(annotation).getOrElse(""))
      // @Path parameters missing from the template are appended as trailing captures
      val appended = pathParamNames(method).count(n => !path.captures.contains(n))
      Route(
        method = method,
        annotation = kind,
        path = path,
        specificity = path.specificityKey(appended),
        docs = EndpointDocs(
          tag = tag,
          summary = method.annotationArg(OkapiAnnotation.Summary),
          description = method.annotationArg(OkapiAnnotation.Description),
          deprecated = method.isAnnotatedWith(OkapiAnnotation.Deprecated),
        ),
      )
    }

    val routes = {
      controller
        .methodMembers
        .flatMap(m => {
          routing(m)
            .map((generator, kind, ann) => generator -> route(m, kind, ann))
        })
    }
    if routes.isEmpty then report.warning(s"Controller ${controller.fullName} has no routed methods")
    // one order across generators: a capture of one kind must not shadow a more specific route of another
    val endpoints = routes.sortBy((_, r) => (r.specificity, r.path.show)).map { (generator, r) =>
      generator.generate(target, r).asExprOf[ServerEndpoint[R, G]]
    }
    Expr.ofList(endpoints)
  }

  /** The routing annotation of `method` (nearest in its lineage) and the generator owning it. */
  private def routing(method: Symbol): Option[(EndpointGenerator, OkapiAnnotation, Term)] = {
    method.nearest { m =>
      m.okapiAnnotations
        .collectFirst(Function.unlift { (kind, term) =>
          generators.find(_.annotations.contains(kind)).map(generator => (generator, kind, term))
        })
    }
  }
}
