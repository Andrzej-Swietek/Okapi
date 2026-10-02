package io.okapi.core.macros.support

import scala.quoted.*
import sttp.tapir.{ Endpoint, EndpointInput, EndpointOutput, Schema }

/** Recognises and builds the type shapes the generator works with: endpoints, transputs, tuples. */
private[okapi] trait TypeShapes extends MacroContext {
  import q.reflect.*

  /** The type parameters of `Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]`. */
  final case class EndpointTypes(
    securityInput: TypeRepr,
    input: TypeRepr,
    error: TypeRepr,
    output: TypeRepr,
    capabilities: TypeRepr,
  ) {
    def all: List[TypeRepr] = List(securityInput, input, error, output, capabilities)
  }

  /** Type parameters read straight off the term's type — valid while the endpoint is being built. */
  def endpointTypes(endpoint: Term): EndpointTypes =
    toEndpointTypes(endpoint.tpe.widenTermRefByName)

  /** Type parameters resolved through `baseType`, for terms whose type may be a subtype of `Endpoint`. */
  def endpointBaseTypes(endpoint: Term): EndpointTypes = {
    val endpointSym = TypeRepr.of[Endpoint[Any, Any, Any, Any, Any]].typeSymbol
    toEndpointTypes(endpoint.tpe.widenTermRefByName.baseType(endpointSym))
  }

  private def toEndpointTypes(tpe: TypeRepr): EndpointTypes = {
    tpe match {
      case AppliedType(_, List(si, i, e, o, c)) => EndpointTypes(si, i, e, o, c)
      case other => abort(s"Unexpected endpoint type shape: ${other.show}")
    }
  }

  /** `T` out of an `EndpointInput[T]` / `EndpointOutput[T]`. */
  def transputValueType(tpe: TypeRepr): TypeRepr = {
    val widened = tpe.widenTermRefByName
    val candidates = List(TypeRepr.of[EndpointInput[Any]], TypeRepr.of[EndpointOutput[Any]])
    candidates
      .map(c => widened.baseType(c.typeSymbol))
      .collectFirst { case AppliedType(_, List(value)) => value }
      .getOrElse(abort(s"Unexpected transput type: ${widened.show}"))
  }

  private def tupleCons: TypeRepr = typeConstructor(TypeRepr.of[*:[Any, EmptyTuple]])

  /** `()` for no types, the type itself for one, an `A *: B *: EmptyTuple` for more. */
  def tupleOf(types: List[TypeRepr]): TypeRepr = {
    types match {
      case Nil => TypeRepr.of[Unit]
      case only :: Nil => only
      case many => many.foldRight(TypeRepr.of[EmptyTuple])((h, t) => AppliedType(tupleCons, List(h, t)))
    }
  }

  /** Flattens one level, as Tapir's `ParamConcat` does: `(A, B)` and `A *: B *: EmptyTuple` → `List(A, B)`, `Unit` →
    * `Nil`, anything else → itself.
    */
  def tupleElements(tpe: TypeRepr): List[TypeRepr] = {
    def flatten(t: TypeRepr): List[TypeRepr] = {
      t.dealias match {
        case AppliedType(cons, List(head, tail)) if cons =:= tupleCons => head :: flatten(tail)
        case empty if empty =:= TypeRepr.of[EmptyTuple] => Nil
        case applied @ AppliedType(_, args) if isTupleClass(applied.typeSymbol) => args
        case other => List(other)
      }
    }
    if tpe =:= TypeRepr.of[Unit] || tpe =:= TypeRepr.of[EmptyTuple] then Nil
    else {
      // Tapir's TupleArity stops at 22: a longer tuple is a single value to it
      val elements = flatten(tpe)
      if elements.size > 22 then List(tpe) else elements
    }
  }

  def isTupleType(tpe: TypeRepr): Boolean = tupleElements(tpe) match {
    case List(single) => !(single =:= tpe)
    case _ => true
  }

  private def isTupleClass(sym: Symbol): Boolean = {
    sym.fullName
      .startsWith("scala.Tuple") && sym.fullName.drop("scala.Tuple".length).toIntOption.exists(n => n >= 1 && n <= 22)
  }

  def concatTuples(left: TypeRepr, right: TypeRepr): TypeRepr =
    tupleOf(tupleElements(left) ++ tupleElements(right))

  /** Summons `Schema[T]`, or derives it via `Mirror` when available. */
  def summonOrDeriveSchema[T: Type]: Option[Expr[Schema[T]]] = {
    Expr.summon[Schema[T]].orElse {
      Expr.summon[scala.deriving.Mirror.Of[T]].map { mirror =>
        '{ Schema.derived[T](using sttp.tapir.generic.Configuration.default, $mirror) }
      }
    }
  }
}
