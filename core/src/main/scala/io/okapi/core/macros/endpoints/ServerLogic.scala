package io.okapi.core.macros.endpoints

import io.okapi.core.OkapiRuntime
import io.okapi.core.http.ApiError.ApiErrorResponse
import io.okapi.core.http.FileResponse
import io.okapi.core.macros.model.{ ArgSlot, InputSource }
import scala.quoted.*

/** Builds the server-logic lambda: Tapir input tuple → controller method call → server effect with mapped errors.
  *
  * Generated shape: `input => OkapiRuntime.serve[C, F, G, A](host, controller => controller.method(args...))`.
  */
private[okapi] trait ServerLogic extends RequestInputs {
  import q.reflect.*

  /** What endpoints are generated for.
    *
    * @param controller
    *   controller type `C`
    * @param effect
    *   `F[_]` its methods return
    * @param server
    *   `G[_]` the server logic runs in
    * @param capabilities
    *   `R` of the resulting `ServerEndpoint[R, G]`
    * @param host
    *   the `ControllerHost[C, F, G]` term
    */
  final case class Target(
    controller: TypeRepr,
    effect: TypeRepr,
    server: TypeRepr,
    capabilities: TypeRepr,
    host: Term,
  )

  def serverLogic(target: Target, spec: MethodSpec, layout: InputLayout): Term = {
    val isFile = spec.output =:= TypeRepr.of[FileResponse]
    val successType = if isFile then TypeRepr.of[(Array[Byte], String)] else spec.output
    val errorType = TypeRepr.of[(sttp.model.StatusCode, ApiErrorResponse)]
    val either = typeConstructor(TypeRepr.of[Either[Any, Any]])
    val hostTypes = List(target.controller, target.effect, target.server)

    Lambda(
      Symbol.spliceOwner,
      MethodType(List("input"))(
        _ => List(inputType(layout)),
        _ => target.server.appliedTo(AppliedType(either, List(errorType, successType))),
      ),
      (owner, inputs) => {
        val (paramArgs, bodyArg) = extractArgs(inputs.head.asInstanceOf[Term], spec, layout)
        val callController = Lambda(
          owner,
          MethodType(List("controller"))(_ => List(target.controller), _ => target.effect.appliedTo(spec.output)),
          (_, controllers) => {
            val call = invokeMethod(controllers.head.asInstanceOf[Term], spec, paramArgs, bodyArg)
            if spec.isEffect then call else callRuntime("pure", hostTypes :+ spec.output, List(target.host, call))
          },
        )
        if isFile then callRuntime("serveFile", hostTypes, List(target.host, callController))
        else callRuntime("serve", hostTypes :+ spec.output, List(target.host, callController))
      },
    )
  }

  /** The value Tapir hands to the server logic: the flat tuple of all inputs, or the tuple of input groups. */
  private def inputType(layout: InputLayout): TypeRepr = {
    if layout.isGrouped then tupleOf(layout.groups.map(_ => TypeRepr.of[OkapiRuntime.InputGroup]))
    else flatType(layout.inputs)
  }

  private def flatType(inputs: List[RequestInput]): TypeRepr =
    inputs.map(_.tpe).foldLeft(TypeRepr.of[Unit])(concatTuples)

  /** Splits Tapir's input value into per-parameter arguments (indexed like `spec.params`) and the body argument. */
  private def extractArgs(input: Term, spec: MethodSpec, layout: InputLayout): (List[Term], Option[Term]) = {
    val values = {
      if !layout.isGrouped then flatValues(input, layout.inputs)
      else {
        layout.groups.zipWithIndex.flatMap { (group, index) =>
          val groupValue = cast(
            productElement(input, index, TypeRepr.of[OkapiRuntime.InputGroup]),
            TypeRepr.of[OkapiRuntime.InputGroup],
          )
          flatValues(Select.unique(groupValue, "values"), group)
        }
      }
    }.toMap
    (spec.params.indices.toList.map(i => values(InputSource.Param(i))), values.get(InputSource.Body))
  }

  /** The values of `inputs` out of the tuple Tapir concatenates them into (`ParamConcat`).
    *
    * Concatenation flattens tuple-typed inputs one level: inputs `Int` and `(Int, String)` arrive as
    * `(Int, Int, String)`. Each input therefore spans `width` positions, and a tuple-typed one is rebuilt from its
    * span; an input spanning everything is the value itself.
    */
  private def flatValues(value: Term, inputs: List[RequestInput]): List[(InputSource, Term)] = {
    val offsets = inputs.map(_.width).scanLeft(0)(_ + _)
    val total = offsets.last
    inputs.zip(offsets).collect {
      case (input, offset) if input.source != InputSource.Fixed =>
        val extracted = {
          if input.width == 0 then cast(Literal(UnitConstant()), input.tpe)
          else if input.width == total then cast(value, input.tpe)
          else if isTupleType(input.tpe) then {
            val elements = tupleElements(input.tpe).zipWithIndex.map((t, i) => productElement(value, offset + i, t))
            cast(tuple(elements), input.tpe)
          }
          else productElement(value, offset, input.tpe)
        }
        input.source -> extracted
    }
  }

  private def productElement(value: Term, position: Int, tpe: TypeRepr): Term = {
    val product = cast(value, TypeRepr.of[Product])
    cast(Apply(Select.unique(product, "productElement"), List(Literal(IntConstant(position)))), tpe)
  }

  /** A tuple of any arity from its elements: `Tuple.fromArray` builds `TupleN` up to 22 and `TupleXXL` beyond. */
  private def tuple(elements: List[Term]): Term = {
    val values = Varargs(elements.map(cast(_, TypeRepr.of[Object]).asExprOf[Object]))
    '{ Tuple.fromArray(Array[Object]($values*)) }.asTerm
  }

  /** `controller.method(clause1)(clause2)...`, with explicit clauses filled from the request. An absent optional
    * parameter gets the method's default value: `controller.method$default$N(<preceding clauses>)`.
    */
  private def invokeMethod(controller: Term, spec: MethodSpec, params: List[Term], body: Option[Term]): Term = {
    def argument(slot: ArgSlot, preceding: List[List[Term]]): Term = {
      slot match {
        case ArgSlot.Body =>
          body.getOrElse(abort(s"Method '${spec.symbol.name}' cannot take a @RequestBody on this kind of endpoint"))
        case ArgSlot.Param(index) =>
          val param = spec.params(index)
          param.default.fold(params(index)) { getter =>
            val default = preceding.foldLeft[Term](Select(controller, getter))(Apply(_, _))
            Apply(TypeApply(Select.unique(params(index), "getOrElse"), List(Inferred(param.tpe))), List(default))
          }
      }
    }
    val argss = spec.clauses.foldLeft(List.empty[List[Term]]) { (preceding, clause) =>
      preceding :+ clause.summoned.getOrElse(clause.slots.map(argument(_, preceding)))
    }
    argss.foldLeft[Term](Select(controller, spec.symbol))(Apply(_, _))
  }

}
