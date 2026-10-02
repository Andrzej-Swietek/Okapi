package io.okapi.codegen

import scala.collection.mutable
import sttp.tapir.codegen.openapi.models.GenerationDirectives
import sttp.tapir.codegen.openapi.models.OpenapiModels.*
import sttp.tapir.codegen.openapi.models.OpenapiSchemaType
import sttp.tapir.codegen.openapi.models.OpenapiSchemaType.*

/** Reads the [[ClientModel]] from an OpenAPI document.
  *
  * Component schemas become models: objects (and `allOf`s of objects, merged) become case classes, string enums become
  * enums, and a `oneOf` with a discriminator becomes a sealed trait over its object components. Inline objects and
  * enums become models named after where they appear. A schema with no Scala shape (`{}`, a `oneOf` without
  * discriminator or object components, an inline `oneOf`) is read as [[TypeRef.RawJson]].
  */
final class ContractReader(document: OpenapiDocument) {

  private val declared: Map[String, OpenapiSchemaType] = document.components.map(_.schemas).getOrElse(Map.empty)

  /** The component schemas, each `allOf` that [[mergeAllOf]] merges replaced by its object. */
  private val schemas: Map[String, OpenapiSchemaType] = declared.map { (name, schema) =>
    name -> (schema match {
      case OpenapiSchemaAllOf(types) => mergeAllOf(types, Set(name)).getOrElse(schema)
      case other => other
    })
  }
  private val sharedParameters = document.components.map(_.parameters).getOrElse(Map.empty)

  private val models = mutable.LinkedHashMap.empty[String, Model]

  /** Per case: its sealed parents, their discriminator field, and the case's value in it. */
  private val parents = mutable.Map.empty[String, List[(Identifier, WireName, String)]].withDefaultValue(Nil)

  /** The model name of each component: its name as an identifier, suffixed when [[Reserved.types]] has it. */
  private val modelNames: Map[String, Identifier] = {
    schemas.keys.toList.sorted.foldLeft(Map.empty[String, Identifier]) { (named, component) =>
      val base = Identifier.tpeOrConverted(component)
      val wanted = if (Reserved.types.contains(base.bare)) base + "Model" else base
      named + (component -> Identifier.unique(wanted, named.values.toSet))
    }
  }

  /** @param rootTrait
    *   the trait of the group with [[Grouping.Single]], and a name the controller traits do not take.
    */
  def read(grouping: Grouping, rootTrait: Identifier): ClientModel = {
    val components = schemas.toList.sortBy(_._1)
    components.foreach {
      case (name, OpenapiSchemaOneOf(types, Some(discriminator))) =>
        val mapped = discriminator.mapping.getOrElse(Map.empty).map((value, ref) => ref.stripPrefix(SchemaRef) -> value)
        objectCases(types).foreach { component =>
          val c = modelName(component)
          val value = mapped.getOrElse(component, component)
          parents(c.value) = parents(c.value) :+ (modelName(name), WireName(discriminator.propertyName), value)
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
        operations.groupBy(_._1).toList.sortBy(_._1).foldLeft(List.empty[Group]) {
          case (done, (tag, ops)) =>
            val name = Identifier.tpe(Identifier.tpe(tag).bare.stripSuffix("Controller") match {
              case "" => tag
              case stripped => stripped
            })
            val traitName = Identifier.unique(name + suffix, done.map(_.traitName).toSet + rootTrait, suffix = "Client")
            val accessor = Identifier.unique(Identifier.term(name.bare), done.map(_.accessor).toSet ++ ReservedMembers)
            done :+ Group(tag, traitName, accessor, uniqueNames(ops.map(op => withoutTag(op._2, tag))))
        }
      case Grouping.Single => List(Group("", rootTrait, rootTrait, uniqueNames(operations.map(_._2))))
    }
    ClientModel(groups, models.values.toList)
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
      done :+ op.copy(name = Identifier.unique(op.name, done.map(_.name).toSet ++ ReservedMembers))
    }
  }

  private def operation(path: OpenapiPath, method: OpenapiPathMethod): Operation = {
    val name = Identifier.term(method.operationId.getOrElse(s"${method.methodType}-${path.url}"))
    val segments = path.url.split('/').toList.filter(_.nonEmpty).map(pathParts)
    val captures = segments.flatten.collect { case Left(wire) => wire }
    val parameters = method.resolvedParameters
      .filter(p => Set("path", "query", "header").contains(p.in))
      .sortBy(p => if (p.in == "path") captures.indexOf(p.name) else Int.MaxValue)

    val taken = mutable.Set.from(Reserved.parameters.map(Identifier.term))
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
      path = segments.map { parts =>
        PathSegment(parts.map {
          case Left(wire) => PathPart.Capture(params.find(_.wire.value == wire).fold(Identifier.term(wire))(_.name))
          case Right(literal) => PathPart.Literal(literal)
        })
      },
      params = pathParams ++ required ++ withDefaults,
      body =
        method.requestBody.map(_.resolve(document)).flatMap(requestBody(name.bare, _, claim, streamsRequest(method))),
      result = result(name.bare, method.responses, streamsResponse(method)),
      summary = method.summary,
    )
  }

  /** The captures (`Left`) and literal text (`Right`) of a path segment: `{name}.json` → `Left(name), Right(.json)`. */
  private def pathParts(segment: String): List[Either[String, String]] = {
    def literal(from: Int, to: Int) = if (to > from) List(Right(segment.substring(from, to))) else Nil
    val (parts, end) = CapturePattern.findAllMatchIn(segment).foldLeft((List.empty[Either[String, String]], 0)) {
      case ((done, at), found) => (done ++ literal(at, found.start) :+ Left(found.group(1)), found.end)
    }
    parts ++ literal(end, segment.length)
  }

  private val CapturePattern = "\\{([^{}]+)\\}".r

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

  private val SchemaRef = "#/components/schemas/"

  private val ReservedMembers: Set[Identifier] = Reserved.members.map(Identifier.term)

  private def modelName(component: String): Identifier =
    modelNames.getOrElse(component, Identifier.tpeOrConverted(component))

  /** The object components among `types`: the cases of a sealed trait over them. */
  private def objectCases(types: Seq[OpenapiSchemaType]): List[String] = {
    val components = types.toList.collect { case ref: OpenapiSchemaRef if ref.isSchema => ref.stripped }
    components.filter(c => schemas.get(c).exists(_.isInstanceOf[OpenapiSchemaObject])).distinct
  }

  /** Whether `schema` is a model: an object, a string enum, or a `oneOf` with a discriminator and object cases. */
  private def isModel(schema: OpenapiSchemaType): Boolean = schema match {
    case _: OpenapiSchemaObject | _: OpenapiSchemaEnum => true
    case OpenapiSchemaOneOf(types, Some(_)) => objectCases(types).nonEmpty
    case _ => false
  }

  /** `types` merged into one object, when each is an object, an object component or such an `allOf`; `seen` are the
    * components being merged.
    */
  private def mergeAllOf(types: Seq[OpenapiSchemaType], seen: Set[String]): Option[OpenapiSchemaObject] = {
    val objects = types.toList.map {
      case obj: OpenapiSchemaObject => Some(obj)
      case ref: OpenapiSchemaRef if ref.isSchema && !seen.contains(ref.stripped) =>
        declared.get(ref.stripped).flatMap {
          case obj: OpenapiSchemaObject => Some(obj)
          case OpenapiSchemaAllOf(inner) => mergeAllOf(inner, seen + ref.stripped)
          case _ => None
        }
      case OpenapiSchemaAllOf(inner) => mergeAllOf(inner, seen)
      case _ => None
    }
    if (objects.isEmpty || objects.contains(None)) None
    else {
      val all = objects.flatten
      val properties = mutable.LinkedHashMap.from(all.flatMap(_.properties))
      Some(OpenapiSchemaObject(properties, all.flatMap(_.required).distinct, all.forall(_.nullable)))
    }
  }

  /** Records the model of the component (or inline schema) `name`, when the schema has a Scala shape of its own. */
  private def declare(name: Identifier, schema: OpenapiSchemaType): Unit = schema match {
    case obj: OpenapiSchemaObject => models(name.value) = record(name, obj)
    case e: OpenapiSchemaEnum => models(name.value) = Model.StringEnum(name, enumValues(e))
    case OpenapiSchemaOneOf(types, Some(discriminator)) if objectCases(types).nonEmpty =>
      models(name.value) = Model.Sealed(name, WireName(discriminator.propertyName), objectCases(types).map(modelName))
    case _ =>
  }

  private def enumValues(e: OpenapiSchemaEnum): List[EnumValue] = {
    e.items.toList.foldLeft(List.empty[EnumValue]) { (done, item) =>
      done :+ EnumValue(Identifier.unique(Identifier.tpe(item.value), done.map(_.name).toSet), item.value)
    }
  }

  private def record(name: Identifier, obj: OpenapiSchemaObject): Model.Record = {
    val discriminators = parents(name.value).map(_._2.value).toSet
    val properties = obj.properties.toList.filterNot((wire, _) => discriminators.contains(wire))
    val fields = properties.foldLeft(List.empty[Field]) {
      case (done, (wire, field)) =>
        val tpe = typeOf(field.`type`, name.bare + Identifier.tpe(wire).bare)
        val required = obj.required.contains(wire) && !field.`type`.nullable
        val (fieldType, default) = if (required) (tpe, None) else optional(tpe)
        val term = Identifier.term(wire)
        val wanted = if (Reserved.fields.contains(term.bare)) term + "Value" else term
        done :+ Field(Identifier.unique(wanted, done.map(_.name).toSet), WireName(wire), fieldType, default)
    }
    val discriminator = parents(name.value).headOption.map(_._3).filter(_ != name.bare).map(WireName(_))
    Model.Record(name, fields, parents(name.value).map(_._1), discriminator)
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
          case Some(component) if isModel(component) => TypeRef.Named(modelName(ref.stripped).value)
          case Some(other) => typeOf(other, modelName(ref.stripped).bare)
          case None => TypeRef.Named(TypeRef.RawJson)
        }
      case OpenapiSchemaArray(items, _, _, restrictions) =>
        TypeRef.Seq(typeOf(items, hint + "Item"), restrictions.uniqueItems.contains(true))
      case OpenapiSchemaMap(items, _, _) => TypeRef.Dict(typeOf(items, hint + "Value"))
      case OpenapiSchemaAllOf(Seq(single)) => typeOf(single, hint)
      case OpenapiSchemaAllOf(types) =>
        mergeAllOf(types, Set.empty).fold(TypeRef.Named(TypeRef.RawJson))(typeOf(_, hint))
      case inline @ (_: OpenapiSchemaObject | _: OpenapiSchemaEnum) =>
        val taken =
          (models.keySet.toSet ++ modelNames.values.map(_.value) ++ Reserved.types).map(Identifier.tpeOrConverted)
        val name = Identifier.unique(Identifier.tpe(hint), taken)
        declare(name, inline)
        TypeRef.Named(name.value)
      case _ => TypeRef.Named(TypeRef.RawJson)
    }
    if (schema.nullable) TypeRef.Opt(tpe) else tpe
  }
}
