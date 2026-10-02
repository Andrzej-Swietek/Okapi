package io.okapi.codegen

import scala.collection.mutable
import sttp.tapir.codegen.openapi.models.GenerationDirectives
import sttp.tapir.codegen.openapi.models.OpenapiModels.*
import sttp.tapir.codegen.openapi.models.OpenapiSchemaType
import sttp.tapir.codegen.openapi.models.OpenapiSchemaType.*

/** Reads the [[ClientModel]] from an OpenAPI document.
  *
  * Component schemas become models: objects become case classes, string enums become enums, and a `oneOf` with a
  * discriminator becomes a sealed trait over its cases. Inline objects and enums become models named after where they
  * appear. A schema with no Scala shape (`{}`, a `oneOf` without discriminator) is read as [[TypeRef.RawJson]].
  */
final class ContractReader(document: OpenapiDocument) {

  private val schemas: Map[String, OpenapiSchemaType] = document.components.map(_.schemas).getOrElse(Map.empty)
  private val sharedParameters = document.components.map(_.parameters).getOrElse(Map.empty)

  private val models = mutable.LinkedHashMap.empty[String, Model]
  private val parents = mutable.Map.empty[String, List[(Identifier, WireName)]].withDefaultValue(Nil)

  /** @param rootTrait
    *   the trait of the group with [[Grouping.Single]], and a name the controller traits do not take.
    */
  def read(grouping: Grouping, rootTrait: Identifier): ClientModel = {
    val components = schemas.toList.sortBy(_._1)
    components.foreach {
      case (name, OpenapiSchemaOneOf(types, Some(discriminator))) =>
        caseNames(types).foreach { c =>
          parents(c.value) = parents(c.value) :+ (modelName(name) -> WireName(discriminator.propertyName))
        }
      case _ =>
    }
    components.foreach((name, schema) => declare(modelName(name), schema))

    val operations = document.paths.toList.flatMap { path =>
      path.methods.map { m =>
        tagOf(m) -> operation(path, m.withResolvedParentParameters(sharedParameters, path.parameters))
      }
    }
    val groups = grouping match {
      case Grouping.ByController(suffix) =>
        operations.groupBy(_._1).toList.sortBy(_._1).map { (tag, ops) =>
          val name = Identifier.tpe(Identifier.tpe(tag).bare.stripSuffix("Controller") match {
            case "" => tag
            case stripped => stripped
          })
          val traitName = Identifier.unique(name + suffix, Set(rootTrait), suffix = "Client")
          Group(tag, traitName, Identifier.term(name.bare), uniqueNames(ops.map(op => withoutTag(op._2, tag))))
        }
      case Grouping.Single => List(Group("", rootTrait, rootTrait, uniqueNames(operations.map(_._2))))
    }
    ClientModel(groups, markRecursive(models.values.toList))
  }

  /** `booksStats` in `Books` → `stats`. */
  private def withoutTag(operation: Operation, tag: String): Operation = {
    val prefix = Identifier.term(tag).bare
    val name = operation.name.bare
    if (name.length > prefix.length && name.startsWith(prefix) && name.charAt(prefix.length).isUpper)
      operation.copy(name = Identifier.term(name.substring(prefix.length)))
    else operation
  }

  private def tagOf(method: OpenapiPathMethod): String = method.tags.flatMap(_.headOption).getOrElse("Default")

  private def uniqueNames(operations: List[Operation]): List[Operation] = {
    operations.foldLeft(List.empty[Operation]) { (done, op) =>
      done :+ op.copy(name = Identifier.unique(op.name, done.map(_.name).toSet))
    }
  }

  private def operation(path: OpenapiPath, method: OpenapiPathMethod): Operation = {
    val name = Identifier.term(method.operationId.getOrElse(s"${method.methodType}-${path.url}"))
    val segments = path.url.split('/').toList.filter(_.nonEmpty).map { segment =>
      if (segment.startsWith("{") && segment.endsWith("}")) Left(segment.drop(1).dropRight(1)) else Right(segment)
    }
    val captures = segments.collect { case Left(wire) => wire }
    val parameters = method.resolvedParameters
      .filter(p => Set("path", "query", "header").contains(p.in))
      .sortBy(p => if (p.in == "path") captures.indexOf(p.name) else Int.MaxValue)

    val taken = mutable.Set.empty[Identifier]
    def claim(wanted: Identifier): Identifier = {
      val chosen = Identifier.unique(wanted, taken.toSet)
      taken += chosen
      chosen
    }
    val params = parameters.toList.map { p =>
      val location = p.in match {
        case "path" => ParamLocation.Path
        case "query" => ParamLocation.Query
        case _ => ParamLocation.Header
      }
      val tpe = typeOf(p.schema, name.bare + Identifier.tpe(p.name).bare)
      val required = location == ParamLocation.Path || p.required.contains(true)
      val (paramType, default) = if (required) (tpe, None) else optional(tpe)
      Param(claim(Identifier.term(p.name)), WireName(p.name), location, paramType, default)
    }
    val (pathParams, others) = params.partition(_.location == ParamLocation.Path)
    val (withDefaults, required) = others.partition(_.default.isDefined)
    Operation(
      name = name,
      method = HttpMethod.valueOf(method.methodType.toLowerCase.capitalize),
      path = segments.map {
        case Left(wire) => PathSegment.Capture(params.find(_.wire.value == wire).fold(Identifier.term(wire))(_.name))
        case Right(literal) => PathSegment.Literal(literal)
      },
      params = pathParams ++ required ++ withDefaults,
      body =
        method.requestBody.map(_.resolve(document)).flatMap(requestBody(name.bare, _, claim, streamsRequest(method))),
      result = result(name.bare, method.responses, streamsResponse(method)),
      summary = method.summary,
    )
  }

  private def streamsRequest(method: OpenapiPathMethod): Boolean =
    method.tapirCodegenDirectives.exists(
      Set(GenerationDirectives.forceStreaming, GenerationDirectives.forceReqStreaming)
    )

  private def streamsResponse(method: OpenapiPathMethod): Boolean =
    method.tapirCodegenDirectives.exists(
      Set(GenerationDirectives.forceStreaming, GenerationDirectives.forceRespStreaming)
    )

  private def requestBody(
    operation: String,
    body: OpenapiRequestBodyDefn,
    claim: Identifier => Identifier,
    streamed: Boolean,
  ): Option[RequestBody] = {
    def preference(contentType: String) =
      if (isJson(contentType)) 0 else if (contentType.startsWith("multipart/")) 1 else 2
    body.content.sortBy(c => preference(c.contentType)).headOption.map { content =>
      val contentType = content.contentType.toLowerCase
      if (isJson(contentType)) {
        val tpe = typeOf(content.schema, operation + "Request")
        val name = tpe match {
          case TypeRef.Named(model) if models.contains(model) => Identifier.term(model)
          case _ => Identifier.term("body")
        }
        RequestBody.Json(claim(name), tpe)
      }
      else if (contentType.startsWith("text/")) RequestBody.Text(claim(Identifier.term("body")))
      else if (contentType == "multipart/form-data" || contentType == "application/x-www-form-urlencoded") {
        val fields = objectOf(content.schema).toList.flatMap { obj =>
          obj.properties.toList.map { (wire, field) =>
            val binary = field.`type`.isInstanceOf[OpenapiSchemaBinary]
            val tpe = if (binary) TypeRef.Bytes else typeOf(field.`type`, operation + Identifier.tpe(wire).bare)
            val (fieldType, default) = if (obj.required.contains(wire)) (tpe, None) else optional(tpe)
            FormField(claim(Identifier.term(wire)), WireName(wire), fieldType, default, binary)
          }
        }
        RequestBody.Form(fields, multipart = contentType.startsWith("multipart/"))
      }
      else if (streamed) RequestBody.Stream(claim(Identifier.term("body")), content.contentType)
      else RequestBody.Bytes(claim(Identifier.term("body")), content.contentType)
    }
  }

  private def result(operation: String, responses: Seq[OpenapiResponse], streamed: Boolean): ResultBody = {
    responses.filter(_.code.startsWith("2")).sortBy(_.code).headOption.map(_.resolve(document)) match {
      case None => ResultBody.Empty
      case Some(response) =>
        response.content.sortBy(c => if (isJson(c.contentType)) 0 else 1).headOption match {
          case None => ResultBody.Empty
          case Some(c) if isJson(c.contentType) => ResultBody.Json(typeOf(c.schema, operation + "Response"))
          case Some(c) if c.contentType.toLowerCase.startsWith("text/event-stream") => ResultBody.Events
          case Some(_) if streamed => ResultBody.Stream
          case Some(c) if c.contentType.toLowerCase.startsWith("text/") => ResultBody.Text
          case Some(_) => ResultBody.Bytes
        }
    }
  }

  private def isJson(contentType: String): Boolean = {
    val lower = contentType.toLowerCase
    lower == "application/json" || lower.endsWith("+json")
  }

  private def objectOf(schema: OpenapiSchemaType): Option[OpenapiSchemaObject] = schema match {
    case obj: OpenapiSchemaObject => Some(obj)
    case ref: OpenapiSchemaRef => schemas.get(ref.stripped).flatMap(objectOf)
    case _ => None
  }

  /** A value that may be left out: a collection defaults to empty, anything else becomes an `Option`. */
  private def optional(tpe: TypeRef): (TypeRef, Option[String]) = tpe match {
    case opt: TypeRef.Opt => (opt, Some("None"))
    case seq @ TypeRef.Seq(_, unique) => (seq, Some(if (unique) "Set.empty" else "Nil"))
    case dict: TypeRef.Dict => (dict, Some("Map.empty"))
    case other => (TypeRef.Opt(other), Some("None"))
  }

  private def modelName(component: String): Identifier = Identifier.tpeOrConverted(component)

  private def caseNames(types: Seq[OpenapiSchemaType]): List[Identifier] =
    types.toList.collect { case ref: OpenapiSchemaRef if ref.isSchema => modelName(ref.stripped) }

  /** Records the model of the component (or inline schema) `name`, when the schema has a Scala shape of its own. */
  private def declare(name: Identifier, schema: OpenapiSchemaType): Unit = schema match {
    case obj: OpenapiSchemaObject => models(name.value) = record(name, obj)
    case e: OpenapiSchemaEnum => models(name.value) = Model.StringEnum(name, enumValues(e))
    case OpenapiSchemaOneOf(types, Some(discriminator)) =>
      models(name.value) = Model.Sealed(name, WireName(discriminator.propertyName), caseNames(types), recursive = false)
    case _ =>
  }

  private def enumValues(e: OpenapiSchemaEnum): List[EnumValue] = {
    e.items.toList.foldLeft(List.empty[EnumValue]) { (done, item) =>
      done :+ EnumValue(Identifier.unique(Identifier.tpe(item.value), done.map(_.name).toSet), item.value)
    }
  }

  private def record(name: Identifier, obj: OpenapiSchemaObject): Model.Record = {
    val discriminators = parents(name.value).map(_._2.value).toSet
    val fields = obj.properties.toList.filterNot((wire, _) => discriminators.contains(wire)).map { (wire, field) =>
      val tpe = typeOf(field.`type`, name.bare + Identifier.tpe(wire).bare)
      val required = obj.required.contains(wire) && !field.`type`.nullable
      val (fieldType, default) = if (required) (tpe, None) else optional(tpe)
      Field(Identifier.term(wire), WireName(wire), fieldType, default)
    }
    Model.Record(name, fields, parents(name.value).map(_._1), recursive = false)
  }

  /** The Scala type of `schema`; `hint` names the model of an inline object or enum. */
  private def typeOf(schema: OpenapiSchemaType, hint: String): TypeRef = {
    val tpe = schema match {
      case _: OpenapiSchemaString | _: OpenapiSchemaConstantString | _: OpenapiSchemaByte | _: OpenapiSchemaBinary =>
        TypeRef.Named("String")
      case _: OpenapiSchemaInt => TypeRef.Named("Int")
      case _: OpenapiSchemaLong => TypeRef.Named("Long")
      case _: OpenapiSchemaDouble => TypeRef.Named("Double")
      case _: OpenapiSchemaFloat => TypeRef.Named("Float")
      case _: OpenapiSchemaBoolean => TypeRef.Named("Boolean")
      case _: OpenapiSchemaUUID => TypeRef.Named("java.util.UUID")
      case _: OpenapiSchemaDate => TypeRef.Named("java.time.LocalDate")
      case _: OpenapiSchemaDateTime => TypeRef.Named("java.time.Instant")
      case _: OpenapiSchemaDuration => TypeRef.Named("java.time.Duration")
      case ref: OpenapiSchemaRef if ref.isSchema =>
        schemas.get(ref.stripped) match {
          case Some(_: OpenapiSchemaObject | _: OpenapiSchemaEnum | OpenapiSchemaOneOf(_, Some(_))) =>
            TypeRef.Named(modelName(ref.stripped).value)
          case Some(other) => typeOf(other, modelName(ref.stripped).bare)
          case None => TypeRef.Named(TypeRef.RawJson)
        }
      case OpenapiSchemaArray(items, _, _, restrictions) =>
        TypeRef.Seq(typeOf(items, hint + "Item"), restrictions.uniqueItems.contains(true))
      case OpenapiSchemaMap(items, _, _) => TypeRef.Dict(typeOf(items, hint + "Value"))
      case inline @ (_: OpenapiSchemaObject | _: OpenapiSchemaEnum | OpenapiSchemaOneOf(_, Some(_))) =>
        val taken = (models.keySet.toSet ++ schemas.keySet.map(modelName(_).value)).map(Identifier.tpeOrConverted)
        val name = Identifier.unique(Identifier.tpe(hint), taken)
        declare(name, inline)
        TypeRef.Named(name.value)
      case _ => TypeRef.Named(TypeRef.RawJson)
    }
    if (schema.nullable) TypeRef.Opt(tpe) else tpe
  }

  /** Marks the models that reach themselves through their fields or cases. */
  private def markRecursive(all: List[Model]): List[Model] = {
    val known = all.map(_.name.value).toSet
    val edges: Map[String, Set[String]] = all.map {
      case r: Model.Record => r.name.value -> r.fields.flatMap(_.tpe.names).filter(known).toSet
      case s: Model.Sealed => s.name.value -> s.cases.map(_.value).toSet
      case e: Model.StringEnum => e.name.value -> Set.empty[String]
    }.toMap
    def reachesItself(start: String): Boolean = {
      val seen = mutable.Set.empty[String]
      def visit(name: String): Boolean =
        edges.getOrElse(name, Set.empty).exists(next => next == start || (seen.add(next) && visit(next)))
      visit(start)
    }
    all.map {
      case r: Model.Record => r.copy(recursive = reachesItself(r.name.value))
      case s: Model.Sealed => s.copy(recursive = reachesItself(s.name.value))
      case e => e
    }
  }
}
