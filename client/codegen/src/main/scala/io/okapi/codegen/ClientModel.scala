package io.okapi.codegen

import scala.collection.mutable

/** The client an OpenAPI document describes, in the terms the renderers emit: operations grouped by controller, and the
  * models they exchange.
  */
final case class ClientModel(groups: List[Group], models: List[Model]) {
  val modelNames: Set[String] = models.map(_.name.value).toSet

  def model(name: String): Option[Model] = models.find(_.name.value == name)

  /** Whether `name` is a case of a sealed trait, which has no codec of its own and is derived in each codec using it.
    */
  def isCase(name: String): Boolean = model(name).exists {
    case r: Model.Record => r.parents.nonEmpty
    case _ => false
  }

  /** Whether an operation or a field has the type `name` itself. */
  def isReferenced(name: String): Boolean =
    (operations.flatMap(_.types) ++ models.flatMap(_.types)).exists(_.mentions(name))

  /** Whether the codec of `m` derives a type that reaches itself. */
  def isRecursive(m: Model): Boolean = derivesRecursion(Set(m.name.value))

  /** Whether a codec derived for `tpe` derives a type that reaches itself. */
  def isRecursive(tpe: TypeRef): Boolean = derivesRecursion(tpe.names.filter(isCase))

  private lazy val references: Map[String, Set[String]] = models.map {
    case r: Model.Record => r.name.value -> r.fields.flatMap(_.tpe.names).filter(modelNames).toSet
    case s: Model.Sealed => s.name.value -> s.cases.map(_.value).toSet
    case e: Model.StringEnum => e.name.value -> Set.empty[String]
  }.toMap

  private def reachesItself(start: String): Boolean = {
    val seen = mutable.Set.empty[String]
    def visit(name: String): Boolean =
      references.getOrElse(name, Set.empty).exists(next => next == start || (seen.add(next) && visit(next)))
    visit(start)
  }

  /** Whether `roots`, or a case they derive, reaches itself. */
  private def derivesRecursion(roots: Set[String]): Boolean = {
    val derived = mutable.Set.empty[String]
    def visit(name: String): Unit =
      if (derived.add(name)) references.getOrElse(name, Set.empty).filter(isCase).foreach(visit)
    roots.foreach(visit)
    derived.exists(reachesItself)
  }

  def usesRawJson: Boolean =
    (groups.flatMap(_.operations.flatMap(_.types)) ++ models.flatMap(_.types)).exists(_.mentions(TypeRef.RawJson))

  private def operations: List[Operation] = groups.flatMap(_.operations)

  /** Whether an operation streams its request or response body. */
  def usesStreams: Boolean = operations.exists(op => op.streamsRequest || op.result == ResultBody.Stream)

  def usesEvents: Boolean = operations.exists(_.result == ResultBody.Events)
}

/** The operations of one controller (the operations' first tag), rendered as the trait `traitName` and reached from the
  * root trait by `accessor`.
  */
final case class Group(tag: String, traitName: Identifier, accessor: Identifier, operations: List[Operation])

final case class Operation(
  name: Identifier,
  method: HttpMethod,
  path: List[PathSegment],
  params: List[Param],
  body: Option[RequestBody],
  result: ResultBody,
  summary: Option[String],
) {
  def types: List[TypeRef] = params.map(_.tpe) ++ body.toList.flatMap(_.types) ++ result.types

  def streamsRequest: Boolean = body.exists(_.isInstanceOf[RequestBody.Stream])
}

enum HttpMethod {
  case Get, Post, Put, Patch, Delete, Head, Options

  /** The sttp request method building a request of this method. */
  def sttp: String = toString.toLowerCase
}

/** A `/`-separated segment of a path: literal text and captured parameters, in order. */
final case class PathSegment(parts: List[PathPart])

enum PathPart {
  case Literal(text: String)
  case Capture(param: Identifier)
}

enum ParamLocation {
  case Path, Query, Header
}

/** A path, query or header parameter; one with a `default` is not required. */
final case class Param(name: Identifier, wire: WireName, location: ParamLocation, tpe: TypeRef, default: Option[String])

enum RequestBody {
  case Json(name: Identifier, tpe: TypeRef)
  case Text(name: Identifier)
  case Bytes(name: Identifier, contentType: String)

  /** A binary body sent as a stream. */
  case Stream(name: Identifier, contentType: String)

  /** `multipart/form-data` (`multipart`) or `application/x-www-form-urlencoded`, a method parameter per field. */
  case Form(fields: List[FormField], multipart: Boolean)

  def types: List[TypeRef] = this match {
    case Json(_, tpe) => List(tpe)
    case Form(fields, _) => fields.map(_.tpe)
    case _ => Nil
  }
}

/** A form field; a `binary` field is sent as a file part. */
final case class FormField(name: Identifier, wire: WireName, tpe: TypeRef, default: Option[String], binary: Boolean)

enum ResultBody {
  case Json(tpe: TypeRef)
  case Text, Bytes, Empty

  /** A binary body read as a stream. */
  case Stream

  /** `text/event-stream`: server-sent events. */
  case Events

  def types: List[TypeRef] = this match {
    case Json(tpe) => List(tpe)
    case _ => Nil
  }
}

enum Model {

  /** A case class, or a case object when it has no fields, extends a [[Model.Sealed]] parent and is not a type of its
    * own anywhere ([[ClientModel.isReferenced]]); `discriminator` is its value in the parent's discriminator field when
    * that is not its name.
    */
  case Record(name: Identifier, fields: List[Field], parents: List[Identifier], discriminator: Option[WireName] = None)

  /** A sealed trait whose cases carry their name in the `discriminator` field. */
  case Sealed(name: Identifier, discriminator: WireName, cases: List[Identifier])

  /** An enum of string values. */
  case StringEnum(name: Identifier, values: List[EnumValue])

  def name: Identifier

  def types: List[TypeRef] = this match {
    case r: Record => r.fields.map(_.tpe)
    case _ => Nil
  }
}

final case class Field(name: Identifier, wire: WireName, tpe: TypeRef, default: Option[String])

final case class EnumValue(name: Identifier, value: String)

/** A Scala type of the client: a model, a `scala` type, or a fully qualified name. */
enum TypeRef {
  case Named(name: String)
  case Opt(of: TypeRef)
  case Seq(of: TypeRef, unique: Boolean)
  case Dict(of: TypeRef)

  /** The type with simple names; [[qualifiedNames]] are the imports it needs. */
  def render: String = this match {
    case Named(name) => name.substring(name.lastIndexOf('.') + 1)
    case Opt(of) => s"Option[${of.render}]"
    case Seq(of, unique) => s"${if (unique) "Set" else "List"}[${of.render}]"
    case Dict(of) => s"Map[String, ${of.render}]"
  }

  def names: Set[String] = this match {
    case Named(name) => Set(name)
    case Opt(of) => of.names
    case Seq(of, _) => of.names
    case Dict(of) => of.names
  }

  def qualifiedNames: Set[String] = names.filter(_.contains('.'))

  def mentions(name: String): Boolean = names.contains(name)
}

object TypeRef {
  val RawJson = "RawJson"
  val Bytes: TypeRef = Named("Array[Byte]")
}
