package io.okapi.codegen.render

import scala.collection.mutable
import io.okapi.codegen.*
import Source.{ codecMaker, file, importsOf, Jsoniter }

/** Renders the models with their jsoniter-scala codecs, in the companions.
  *
  * A sealed trait and its cases share a file. The cases have no codec of their own: an implicit case codec would
  * replace the case in the sealed trait's codec, which writes the discriminator.
  */
private[codegen] object ModelRenderer {

  private val Codec = s"$Jsoniter.core.JsonValueCodec"
  private val Maker = s"$Jsoniter.macros.JsonCodecMaker"
  private val Config = s"$Jsoniter.macros.CodecMakerConfig"
  private val Named = s"$Jsoniter.macros.named"
  private val Reader = s"$Jsoniter.core.JsonReader"
  private val Writer = s"$Jsoniter.core.JsonWriter"

  def render(model: ClientModel, packages: Packages, separateFiles: Boolean): List[SourceFile] = {
    val units = families(model.models).map(family => ModelFile(family.head.name.bare, family, rawJson = false))
    val all = units ++ (if (model.usesRawJson) List(ModelFile(TypeRef.RawJson, Nil, rawJson = true)) else Nil)
    if (separateFiles) all.map(u => SourceFile(packages.models.file(u.name), source(List(u), model, packages)))
    else List(SourceFile(packages.models.file("Models"), source(all, model, packages)))
  }

  /** The models of one source file. */
  private case class ModelFile(name: String, models: List[Model], rawJson: Boolean)

  /** The models in source order, each sealed trait followed by its cases; a case of several sealed traits joins them.
    */
  private def families(models: List[Model]): List[List[Model]] = {
    val owner = mutable.Map.empty[String, String]
    def root(name: String): String = owner.get(name).filter(_ != name).fold(name)(root)
    models.foreach {
      case s: Model.Sealed => s.cases.foreach(c => owner(root(c.value)) = root(s.name.value))
      case _ =>
    }
    val byRoot = models.groupBy(m => root(m.name.value))
    models.map(m => root(m.name.value)).distinct.map { head =>
      val (sealedTraits, rest) = byRoot(head).partition(_.isInstanceOf[Model.Sealed])
      sealedTraits ++ rest
    }
  }

  private def source(units: List[ModelFile], model: ClientModel, packages: Packages): String = {
    val models = units.flatMap(_.models)
    val withRawJson = units.exists(_.rawJson)
    val imports = importsOf(models.flatMap(_.types), model, packages) ++
      (if (models.exists(needsMaker)) List(Codec, Maker, Config) else Nil) ++
      (if (models.exists(renamesFields)) List(Named) else Nil) ++
      (if (models.exists(_.isInstanceOf[Model.StringEnum]) || withRawJson) List(Codec, Reader, Writer) else Nil)
    file(packages.models, imports, models.map(definition(_, model)) ++ (if (withRawJson) List(RawJson) else Nil))
  }

  private def needsMaker(m: Model): Boolean = m match {
    case r: Model.Record => r.parents.isEmpty
    case _: Model.Sealed => true
    case _: Model.StringEnum => false
  }

  private def renamesFields(m: Model): Boolean = m match {
    case r: Model.Record => r.discriminator.isDefined || r.fields.exists(f => f.name.bare != f.wire.value)
    case _ => false
  }

  private def definition(m: Model, model: ClientModel): String = m match {
    case r: Model.Record => record(r, model)
    case s: Model.Sealed =>
      val name = s.name.value
      val make = codecMaker(model.isRecursive(s), Some(s.discriminator))
      s"sealed trait $name\n\nobject $name {\n  given JsonValueCodec[$name] = $make\n}"
    case e: Model.StringEnum => stringEnum(e)
  }

  private def record(r: Model.Record, model: ClientModel): String = {
    val name = r.name.value
    val parents = if (r.parents.isEmpty) "" else r.parents.map(_.value).mkString(" extends ", " with ", "")
    val discriminator = r.discriminator.fold("")(value => s"@named(${value.literal})\n")
    if (r.fields.isEmpty && r.parents.nonEmpty && !model.isReferenced(name))
      s"${discriminator}case object $name$parents"
    else {
      val fields = r.fields.map { f =>
        val renamed = if (f.name.bare != f.wire.value) s"@named(${f.wire.literal}) " else ""
        s"$renamed${f.name.value}: ${f.tpe.render}${f.default.fold("")(" = " + _)}"
      }
      val declaration = s"${discriminator}final case class $name(${fields.mkString(", ")})$parents"
      if (r.parents.nonEmpty) declaration
      else {
        s"$declaration\n\nobject $name {\n  given JsonValueCodec[$name] = ${codecMaker(model.isRecursive(r))}\n}"
      }
    }
  }

  private def stringEnum(e: Model.StringEnum): String = {
    val name = e.name.value
    val cases = e.values.map(v => s"  case ${v.name.value} extends $name(${Literal(v.value)})")
    s"""enum $name(val value: String) {
       |${cases.mkString("\n")}
       |
       |  override def toString: String = value
       |}
       |
       |object $name {
       |  given JsonValueCodec[$name] = new JsonValueCodec[$name] {
       |    def nullValue: $name = null.asInstanceOf[$name]
       |
       |    def decodeValue(in: JsonReader, default: $name): $name = {
       |      val value = in.readString("")
       |      values.find(_.value == value).getOrElse(in.decodeError(s"unknown $name: $$value"))
       |    }
       |
       |    def encodeValue(x: $name, out: JsonWriter): Unit = out.writeVal(x.value)
       |  }
       |}""".stripMargin
  }

  private val RawJson: String = {
    """/** A JSON value the client does not model, as its JSON text. */
      |final case class RawJson(json: String)
      |
      |object RawJson {
      |  given JsonValueCodec[RawJson] = new JsonValueCodec[RawJson] {
      |    def nullValue: RawJson = null.asInstanceOf[RawJson]
      |
      |    def decodeValue(in: JsonReader, default: RawJson): RawJson =
      |      RawJson(new String(in.readRawValAsBytes(), java.nio.charset.StandardCharsets.UTF_8))
      |
      |    def encodeValue(x: RawJson, out: JsonWriter): Unit =
      |      out.writeRawVal(x.json.getBytes(java.nio.charset.StandardCharsets.UTF_8).nn)
      |  }
      |}""".stripMargin
  }
}
