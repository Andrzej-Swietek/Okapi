package io.okapi.core.macros.endpoints

import io.okapi.core.macros.codecs.InputCodecs
import io.okapi.core.macros.model.{ InputSource, ParamKind, RoutePath }

/** Builds a method's request inputs — path template, annotated parameters, body — and lays them out for Tapir. */
private[okapi] trait RequestInputs extends EndpointDsl with InputCodecs {
  import q.reflect.*

  /** One Tapir input. `tpe` is the value it decodes; it spans `width` positions of the flat input tuple. */
  final case class RequestInput(term: Term, source: InputSource, tpe: TypeRepr) {
    def width: Int = tupleElements(tpe).size
  }

  /** How the inputs reach Tapir: one flat tuple for at most 22 values (the most Tapir's `ParamConcat` flattens),
    * otherwise consecutive groups of at most 22, each one [[io.okapi.core.OkapiRuntime.InputGroup]]. Inputs keep their
    * order in both layouts.
    */
  final case class InputLayout(groups: List[List[RequestInput]]) {
    def isGrouped: Boolean = groups.size > 1
    def inputs: List[RequestInput] = groups.flatten
  }

  private val MaxTupleArity = 22

  /** Walks the URL template (fixed segments and `{captures}` in URL order), then every remaining parameter — `@Path`
    * ones absent from the template and all non-path ones — in declaration order.
    */
  def requestInputs(path: RoutePath, spec: MethodSpec): List[RequestInput] = {
    val indexed = spec.params.zipWithIndex
    val pathParams = indexed.filter(_._1.kind == ParamKind.Path).groupBy(_._1.name)
    pathParams.find(_._2.size > 1).foreach { (name, _) =>
      abort(s"Method '${spec.symbol.name}' declares @Path(\"$name\") more than once")
    }
    def param(index: Int): RequestInput =
      RequestInput(paramInput(spec.params(index)).asTerm, InputSource.Param(index), spec.params(index).inputType)

    val (templated, captured) = path.segments.foldLeft((Vector.empty[RequestInput], Set.empty[Int])) {
      case ((inputs, captured), RoutePath.Segment.Fixed(segment)) =>
        (inputs :+ RequestInput(fixedPathInput(segment).asTerm, InputSource.Fixed, TypeRepr.of[Unit]), captured)
      case ((inputs, captured), RoutePath.Segment.Capture(name)) =>
        val index = pathParams.get(name).flatMap(_.headOption).map(_._2).getOrElse {
          abort(
            s"Path template '${path.show}' contains {$name} but no @Path(\"$name\") parameter in method '${spec.symbol.name}'"
          )
        }
        if captured.contains(index) then abort(s"Path template '${path.show}' repeats {$name}")
        (inputs :+ param(index), captured + index)
    }
    templated.toList ++ spec.params.indices.filterNot(captured.contains).map(param)
  }

  def bodyInput(tpe: TypeRepr, mediaType: Option[String]): RequestInput =
    RequestInput(requestBodyInput(tpe, mediaType).asTerm, InputSource.Body, tpe)

  def layout(inputs: List[RequestInput]): InputLayout = {
    if inputs.map(_.width).sum <= MaxTupleArity then InputLayout(List(inputs))
    else {
      val groups = inputs.foldLeft(Vector.empty[Vector[RequestInput]]) { (groups, input) =>
        groups.lastOption match {
          case Some(last) if last.map(_.width).sum + input.width <= MaxTupleArity => groups.init :+ (last :+ input)
          case _ => groups :+ Vector(input)
        }
      }
      if groups.size > MaxTupleArity then
        abort(s"More than ${MaxTupleArity * MaxTupleArity} request inputs; declare fewer parameters")
      InputLayout(groups.map(_.toList).toList)
    }
  }

  def applyInputs(endpoint: Term, layout: InputLayout): Term = {
    if !layout.isGrouped then endpoint.withInputs(layout.inputs.map(_.term))
    else endpoint.withInputs(layout.groups.map(group))
  }

  /** `OkapiRuntime.grouped(input1.and(input2)...)`: one `InputGroup` value. */
  private def group(inputs: List[RequestInput]): Term = {
    val combined = inputs.map(_.term).reduceLeft(_.and(_))
    callRuntime("grouped", List(transputValueType(combined.tpe)), List(combined))
  }
}
