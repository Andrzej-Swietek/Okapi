package io.okapi.core.macros

import zio.{ RIO, Scope, Tag, ZIO }

import io.okapi.core.OkapiZioRuntime
import io.okapi.core.ZioBackend.{ EnvironmentHost, Handler, ScopedEnvironmentHost }
import io.okapi.core.macros.json.ZioJsonDefault
import io.okapi.core.macros.streaming.{ WebSocketEndpoints, ZioStreamBodies }
import io.okapi.core.macros.support.TypeLists
import scala.quoted.*
import sttp.capabilities.WebSockets
import sttp.tapir.server.ziohttp.ZioHttpServerOptions
import sttp.tapir.ztapir.ZServerEndpoint

/** The core endpoint macro specialised for ZIO: `ZStream` bodies, `@WebSocket` endpoints, and controller methods
  * returning any `ZIO[R, ApiError | Throwable, A]`.
  *
  * The environment `R` of every routed method is collected into the endpoints' type: `endpoints[C]` is a
  * `List[ZServerEndpoint[C & R1 & R2 ..., WebSockets]]`, so a missing layer is a compile error at `.provide`. A `Scope`
  * requirement is not propagated: such controllers get a fresh scope per request.
  */
private[okapi] final class ZioEndpointsMacro(quotes: Quotes)
  extends EndpointsMacro(quotes)
    with ZioStreamBodies
    with WebSocketEndpoints
    with ZioJsonDefault
    with TypeLists {
  import q.reflect.*

  override protected def generators: List[EndpointGenerator] = super.generators :+ WebSocketGenerator

  override def effectDescription(effect: TypeRepr): String = "ZIO[R, ApiError | Throwable, A]"

  /** What controller `C`'s routed methods need from the ZIO environment besides `C` itself. */
  final case class Requirements(controller: TypeRepr, extra: List[TypeRepr], scoped: Boolean) {
    def extraType: TypeRepr = intersection(extra)
    def environment: TypeRepr = intersection(controller :: extra)
  }

  def requirements(controller: TypeRepr): Requirements = {
    val needed = routedMethods(controller).flatMap(m => environmentOf(resultType(controller, m))).flatMap(conjuncts)
    val (scopes, services) = needed.partition(_ =:= TypeRepr.of[Scope])
    // the controller itself (or a supertype of it) is found in the environment through `C`
    Requirements(controller, distinct(services.filterNot(controller <:< _)), scopes.nonEmpty)
  }

  /** `Routes[C & Extra, Response]` for controller `C`. */
  def controllerRoutes[C: Type](options: Expr[ZioHttpServerOptions[Any]]): Expr[Any] =
    routes(controllerEndpoints[C], requirements(TypeRepr.of[C]).environment, options)

  /** `Routes[C1 & C2 & ... & Extra, Response]` for the controllers in tuple `Types`. */
  def selectedRoutes[Types: Type](options: Expr[ZioHttpServerOptions[Any]]): Expr[Any] =
    routes(selectedEndpoints[Types], environmentOfAll(tupleMembers(TypeRepr.of[Types])), options)

  /** `List[ZServerEndpoint[C & Extra, WebSockets]]` for controller `C`, sharing one host instance. */
  def controllerEndpoints[C: Type]: Expr[List[Any]] = {
    val required = requirements(TypeRepr.of[C])
    val tag = Expr.summon[Tag[C]].getOrElse {
      abort(
        s"No zio.Tag for controller ${TypeRepr.of[C].show}: its endpoints cannot look it up in the ZIO environment. " +
          "Use a concrete controller type, or require a Tag for its type parameters."
      )
    }
    (required.extraType.asType, required.environment.asType) match {
      case ('[extra], '[env]) =>
        val endpoints = {
          if required.scoped then {
            '{
              val host = ScopedEnvironmentHost[C, extra](using $tag)
              ${
                ZioEndpointsMacro(summon[Quotes])
                  .expand[C, [x] =>> Handler[C & extra & Scope, x], [x] =>> RIO[C & extra, x], WebSockets]('host)
              }
            }
          }
          else {
            '{
              val host = EnvironmentHost[C, extra](using $tag)
              ${
                ZioEndpointsMacro(summon[Quotes])
                  .expand[C, [x] =>> Handler[C & extra, x], [x] =>> RIO[C & extra, x], WebSockets]('host)
              }
            }
          }
        }
        ascribe(endpoints, TypeRepr.of[List[ZServerEndpoint[env, WebSockets]]])
    }
  }

  /** All endpoints of the controllers in tuple `Types`, typed with the union of their requirements. */
  def selectedEndpoints[Types: Type]: Expr[List[Any]] = {
    val controllers = tupleMembers(TypeRepr.of[Types])
    val lists = controllers.map(_.asType match { case '[c] => controllerEndpoints[c] })
    environmentOfAll(controllers).asType match {
      case '[env] =>
        // each controller's endpoints run in RIO[Env_i, *]; Env ⊇ every Env_i, and ZEnvironment lookups are by
        // service, so serving them all in RIO[Env, *] is sound
        val all = '{ List.concat[Any](${ Varargs(lists) }*) }
        ascribe(all, TypeRepr.of[List[ZServerEndpoint[env, WebSockets]]])
    }
  }

  private def environmentOfAll(controllers: List[TypeRepr]): TypeRepr =
    intersection(controllers.map(requirements(_).environment))

  private def routes(endpoints: Expr[List[Any]], environment: TypeRepr, options: Expr[ZioHttpServerOptions[Any]])
    : Expr[Any] = {
    environment.asType match {
      case '[env] =>
        '{ OkapiZioRuntime.toRoutes[env](${ endpoints.asExprOf[List[ZServerEndpoint[env, WebSockets]]] }, $options) }
    }
  }

  /** `R` of a `ZIO[R, E, A]` result type. */
  private def environmentOf(result: TypeRepr): Option[TypeRepr] = {
    result.dealias.simplified match {
      case AppliedType(effect, List(env, _, _)) if effect.typeSymbol == TypeRepr.of[ZIO[Any, Any, Any]].typeSymbol =>
        Some(env)
      case _ => None
    }
  }

  private def ascribe(list: Expr[Any], tpe: TypeRepr): Expr[List[Any]] =
    Typed(cast(list.asTerm, tpe), Inferred(tpe)).asExprOf[List[Any]]
}
