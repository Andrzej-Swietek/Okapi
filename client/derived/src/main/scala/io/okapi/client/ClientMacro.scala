package io.okapi.client

import io.okapi.core.{ OkapiEffect, OkapiRuntime }
import io.okapi.core.http.FileResponse
import io.okapi.core.macros.EndpointsMacro
import io.okapi.core.macros.model.{ ArgSlot, InputSource }
import scala.quoted.*
import scala.reflect.NameTransformer
import sttp.client4.Backend
import sttp.model.Uri

/** Expansion of `OkapiClient[F].of[Api]`: one call per routed method of trait `Api`, dispatched by a
  * [[OkapiClientRuntime.proxy]].
  */
private[okapi] final class ClientMacro(quotes: Quotes) extends EndpointsMacro(quotes) {
  import q.reflect.*

  def expand[F[_]: Type, Api: Type](baseUri: Expr[Uri], backend: Expr[Backend[F]], effect: Expr[OkapiEffect[F]])
    : Expr[Api] = {
    val api = TypeRepr.of[Api]
    if !api.typeSymbol.flags.is(Flags.Trait) then abort(s"OkapiClient needs a trait, got ${api.show}")
    val calls = routes(api).map { (generator, route) =>
      val described = generator.describe(api, TypeRepr.of[F], route)
      if !described.spec.isEffect then {
        abort(s"Client method '${route.method.name}' must return ${TypeRepr.of[F].show}[A]")
      }
      val name = Expr(NameTransformer.encode(route.method.name))
      val parameters = parameterTypes(route.method).map(t => Literal(ClassOfConstant(erasure(t))).asExprOf[Class[?]])
      val call = callOf[F](described, baseUri.asTerm, backend.asTerm, effect.asTerm).asExprOf[OkapiClientRuntime.Call]
      '{ OkapiClientRuntime.Route($name, List(${ Varargs(parameters) }*), $call) }
    }
    val apiClass = Literal(ClassOfConstant(api)).asExprOf[Class[Api]]
    '{ OkapiClientRuntime.proxy[Api]($apiClass, List(${ Varargs(calls) }*)) }
  }

  /** The term parameter types of `method` across all clauses, as declared by its owner. */
  private def parameterTypes(method: Symbol): List[TypeRepr] = {
    def loop(tpe: TypeRepr): List[TypeRepr] = tpe match {
      case MethodType(_, params, result) => params ++ loop(result)
      case _ => Nil
    }
    loop(This(method.owner).tpe.memberType(method))
  }

  /** The type whose class the JVM erases a parameter of type `tpe` to; `Object` for a type it cannot resolve to a
    * class.
    */
  private def erasure(tpe: TypeRepr): TypeRepr = tpe match {
    case ByNameType(_) => TypeRepr.of[Function0[?]]
    case _ =>
      val t = tpe.dealias
      val sym = t.typeSymbol
      if sym == defn.RepeatedParamClass then TypeRepr.of[Seq[?]]
      else if sym == defn.ArrayClass then {
        t match {
          case AppliedType(_, List(element)) if element.dealias.typeSymbol.isClassDef =>
            defn.ArrayClass.typeRef.appliedTo(erasure(element))
          case _ => TypeRepr.of[Object]
        }
      }
      else if !sym.isClassDef then TypeRepr.of[Object]
      else if t <:< TypeRepr.of[AnyVal] && !defn.ScalaPrimitiveValueClasses.contains(sym) then {
        sym.primaryConstructor.paramSymss.flatten.find(_.isTerm) match {
          case Some(field) => erasure(This(sym).tpe.memberType(sym.fieldMember(field.name)))
          case None => TypeRepr.of[Object]
        }
      }
      else t
  }

  /** `{ val endpoint = ...; (args: Array[AnyRef | Null]) => OkapiClientRuntime.call(endpoint, ..., input(args)) }` */
  private def callOf[F[_]: Type](described: Described, baseUri: Term, backend: Term, effect: Term): Term = {
    val types = endpointBaseTypes(described.endpoint)
    val endpointSym =
      Symbol.newVal(Symbol.spliceOwner, "endpoint", described.endpoint.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
    val endpointDef = ValDef(endpointSym, Some(described.endpoint.changeOwner(endpointSym)))
    val lambda = Lambda(
      Symbol.spliceOwner,
      MethodType(List("args"))(_ => List(TypeRepr.of[Array[AnyRef | Null]]), _ => TypeRepr.of[Any]),
      (_, params) => {
        val args = params.head.asInstanceOf[Term]
        val input = cast(inputValue(described, args), types.input)
        val call = callModule(
          "io.okapi.client.OkapiClientRuntime",
          "call",
          List(TypeRepr.of[F], types.input, types.output),
          List(Ref(endpointSym), baseUri, backend, effect, input),
        )
        if described.spec.output =:= TypeRepr.of[FileResponse] then {
          val map = Select.unique(Select.unique(effect, "monad"), "map")
          Apply(
            Apply(TypeApply(map, List(Inferred(types.output), Inferred(TypeRepr.of[FileResponse]))), List(call)),
            List('{ OkapiClientRuntime.fileResponse }.asTerm),
          )
        }
        else call
      },
    )
    Block(List(endpointDef), lambda)
  }

  /** The value Tapir expects for the endpoint's inputs, built from the method's arguments (see [[InputLayout]]). */
  private def inputValue(described: Described, args: Term): Term = {
    val positions = argumentPositions(described.spec)
    def argument(position: Int, tpe: TypeRepr): Term =
      cast(Apply(Select.unique(args, "apply"), List(Literal(IntConstant(position)))), tpe)
    def valueOf(source: InputSource): Option[Term] = {
      source match {
        case InputSource.Fixed => None
        case InputSource.Param(index) =>
          val param = described.spec.params(index)
          val arg = argument(positions(ArgSlot.Param(index)), param.tpe)
          Some(if param.default.isDefined then some(arg, param.tpe) else arg)
        case InputSource.Body => described.spec.body.map(argument(positions(ArgSlot.Body), _))
      }
    }
    def flat(inputs: List[RequestInput]): Term = {
      val elements = inputs.flatMap { input =>
        valueOf(input.source).toList.flatMap { value =>
          if input.width <= 1 then List(value)
          else tupleElements(input.tpe).indices.map(i => productElement(value, i)).toList
        }
      }
      elements match {
        case Nil => Literal(UnitConstant())
        case single :: Nil => single
        case many =>
          '{
            Tuple.fromArray(Array[Object](${ Varargs(many.map(cast(_, TypeRepr.of[Object]).asExprOf[Object])) }*))
          }.asTerm
      }
    }
    if !described.inputs.isGrouped then flat(described.inputs.inputs)
    else {
      val groups = described.inputs.groups.map(group => '{ OkapiRuntime.InputGroup(${ flat(group).asExpr }) }.asTerm)
      '{ Tuple.fromArray(Array[Object](${ Varargs(groups.map(_.asExprOf[Object])) }*)) }.asTerm
    }
  }

  /** Where each argument slot sits among the method's term parameters, all clauses flattened. */
  private def argumentPositions(spec: MethodSpec): Map[ArgSlot, Int] = {
    spec.clauses
      .foldLeft((0, Map.empty[ArgSlot, Int])) {
        case ((offset, positions), clause) =>
          clause.summoned match {
            case Some(summoned) => (offset + summoned.size, positions)
            case None =>
              (
                offset + clause.slots.size,
                positions ++ clause.slots.zipWithIndex.map((slot, i) => slot -> (offset + i)),
              )
          }
      }
      ._2
  }

  private def some(value: Term, tpe: TypeRepr): Term =
    Apply(TypeApply(Select.unique(Ref(Symbol.requiredModule("scala.Some")), "apply"), List(Inferred(tpe))), List(value))

  private def productElement(value: Term, index: Int): Term =
    Apply(Select.unique(cast(value, TypeRepr.of[Product]), "productElement"), List(Literal(IntConstant(index))))
}

private[okapi] object ClientMacro {
  def of[F[_]: Type, Api: Type](
    baseUri: Expr[Uri],
    backend: Expr[Backend[F]],
    effect: Expr[OkapiEffect[F]],
  )(using
    q: Quotes
  ): Expr[Api] =
    ClientMacro(q).expand[F, Api](baseUri, backend, effect)
}
