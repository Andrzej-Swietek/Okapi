package io.okapi.core.macros.endpoints

import io.okapi.core.macros.model.{ EndpointDocs, TransputSlot }
import io.okapi.core.macros.support.TypeShapes
import scala.quoted.*

/** Fluent builder over endpoint terms: `endpoint.withInput(a).withOutput(b).withDocs(docs)`.
  *
  * Each step re-reads the endpoint's current type parameters, so the resulting term is precisely typed.
  */
private[okapi] trait EndpointDsl extends TypeShapes {
  import q.reflect.*

  extension (endpoint: Term) {
    def withInput(input: Term): Term = append(endpoint, input, TransputSlot.Input)

    def withInputs(inputs: List[Term]): Term = inputs.foldLeft(endpoint)(_.withInput(_))

    def withOutput(output: Term): Term = append(endpoint, output, TransputSlot.Output)

    def withErrorOutput(output: Term): Term = append(endpoint, output, TransputSlot.ErrorOutput)

    def withDocs(docs: EndpointDocs): Term = {
      val tagged = invoke(endpoint, "tag", Expr(docs.tag).asTerm)
      val summarised = docs.summary.fold(tagged)(s => invoke(tagged, "summary", Expr(s).asTerm))
      val described = docs.description.fold(summarised)(d => invoke(summarised, "description", Expr(d).asTerm))
      if docs.deprecated then invokeNullary(described, "deprecated") else described
    }
  }

  extension (transput: Term) {

    /** `input.and(other)` for two standalone inputs, with the `ParamConcat` evidence. */
    def and(other: Term): Term = combine("andInput", transput, other)

    /** `output.and(other)` for two standalone outputs, with the `ParamConcat` evidence. */
    def andOutput(other: Term): Term = combine("andOutput", transput, other)
  }

  private def combine(runtimeMethod: String, left: Term, right: Term): Term = {
    val (leftValue, rightValue) = (transputValueType(left.tpe), transputValueType(right.tpe))
    val typeArgs = List(leftValue, rightValue, concatTuples(leftValue, rightValue))
    Apply(callRuntime(runtimeMethod, typeArgs, List(left, right)), List(summonParamConcat(leftValue, rightValue)))
  }

  /** Emits `OkapiRuntime.add{Input,Output,ErrorOutput}` with the `ParamConcat` evidence for the chosen accumulator. */
  private def append(endpoint: Term, transput: Term, slot: TransputSlot): Term = {
    val types = endpointTypes(endpoint)
    val current = slot match {
      case TransputSlot.Input => types.input
      case TransputSlot.Output => types.output
      case TransputSlot.ErrorOutput => types.error
    }
    val next = transputValueType(transput.tpe)
    val typeArgs = types.all ++ List(next, concatTuples(current, next))
    Apply(callRuntime(slot.runtimeMethod, typeArgs, List(endpoint, transput)), List(summonParamConcat(current, next)))
  }

  private def summonParamConcat(left: TypeRepr, right: TypeRepr): Term = {
    // Tapir's instance for (Unit, Unit) is `concatUnitUnit[U]` with an unused type parameter, which implicit search
    // leaves uninstantiated in the tree — the compiler then fails to pickle it. Instantiate it explicitly.
    if left =:= TypeRepr.of[Unit] && right =:= TypeRepr.of[Unit] then
      return '{ sttp.tapir.typelevel.ParamConcat.concatUnitUnit[Unit] }.asTerm
    val paramConcat = typeConstructor(TypeRepr.of[sttp.tapir.typelevel.ParamConcat[Any, Any]])
    Implicits.search(AppliedType(paramConcat, List(left, right))) match {
      case success: ImplicitSearchSuccess => success.tree
      case failure: ImplicitSearchFailure =>
        abort(s"Cannot summon ParamConcat[${left.show}, ${right.show}]: ${failure.explanation}")
    }
  }

  /** Calls the single-argument overload of `methodName` on the endpoint. */
  private def invoke(endpoint: Term, methodName: String, argument: Term): Term = {
    val method = endpointMethods(endpoint, methodName)
      .find(_.paramSymss.find(_.headOption.exists(_.isTerm)).exists(_.size == 1))
      .getOrElse(abort(s"Cannot resolve method '$methodName' on ${endpoint.tpe.show}"))
    Apply(Select(endpoint, method), List(argument))
  }

  private def invokeNullary(endpoint: Term, methodName: String): Term = {
    val method = endpointMethods(endpoint, methodName)
      .headOption
      .getOrElse(abort(s"Cannot resolve zero-arg method '$methodName' on ${endpoint.tpe.show}"))
    Apply(Select(endpoint, method), Nil)
  }

  private def endpointMethods(endpoint: Term, name: String): List[Symbol] =
    endpoint.tpe.widenTermRefByName.typeSymbol.methodMember(name)
}
