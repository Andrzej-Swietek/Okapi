package io.okapi.codegen

/** The client an OpenAPI document describes, in the terms the renderers emit: operations grouped by controller, and the
  * models they exchange.
  */
final case class ClientModel(groups: List[Group], models: List[Model]) {
  val modelNames: Set[String] = models.map(_.name.value).toSet

  def model(name: String): Option[Model] = models.find(_.name.value == name)

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

enum PathSegment {
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

  /** A case class, or a case object when it has no fields and extends a [[Model.Sealed]] parent. */
  case Record(name: Identifier, fields: List[Field], parents: List[Identifier], recursive: Boolean)

  /** A sealed trait whose cases carry their name in the `discriminator` field. */
  case Sealed(name: Identifier, discriminator: WireName, cases: List[Identifier], recursive: Boolean)

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
