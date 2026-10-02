package io.okapi.codegen.render

import io.okapi.codegen.*
import StreamingSyntax.*

/** A generated source; `path` is relative to the source root. */
final case class SourceFile(path: String, content: String)

/** One method parameter: its name, type and default value. */
final case class Argument(name: Identifier, tpe: String, default: Option[String]) {
  def declaration(withDefault: Boolean): String =
    s"${name.value}: $tpe" + (if (withDefault) default.fold("")(" = " + _) else "")
}

private[codegen] object Source {

  val Jsoniter = "com.github.plokhotnyuk.jsoniter_scala"

  /** A source file of `pkg` importing the fully qualified names `imports`, one import per package; names in `pkg` or
    * `scala` are not imported.
    */
  def file(pkg: PackageName, imports: Iterable[String], members: Seq[String]): String = {
    val importLines = imports.toList.distinct
      .filterNot(name => packageOf(name) == pkg.value || packageOf(name) == "scala" || !name.contains('.'))
      .groupBy(packageOf)
      .toList
      .sortBy(_._1)
      .map { (from, names) =>
        names.map(_.substring(from.length + 1)).sorted match {
          case List(single) => s"import $from.$single"
          case many => s"import $from.{ ${many.mkString(", ")} }"
        }
      }
    val header = s"package ${pkg.value}" :: (if (importLines.isEmpty) Nil else "" :: importLines)
    (header ++ members.flatMap(m => List("", m))).mkString("", "\n", "\n")
  }

  /** `JsonCodecMaker.make` writing empty collections, with `discriminator` as the discriminator field and recursive
    * types allowed when `recursive`.
    */
  def codecMaker(recursive: Boolean, discriminator: Option[WireName] = None): String = {
    "JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false)" +
      discriminator.fold("")(d => s".withDiscriminatorFieldName(Some(${d.literal}))") +
      (if (recursive) ".withAllowRecursiveTypes(true)" else "") + ")"
  }

  private def packageOf(name: String): String = name.substring(0, math.max(name.lastIndexOf('.'), 0))

  /** A scaladoc comment over `text`, indented by `indent`. */
  def doc(text: String, indent: String = ""): String = {
    text.trim.replace("*/", "*&#47;").split("\n").toList.map(_.trim) match {
      case List(single) => s"$indent/** $single */\n"
      case lines => lines.mkString(s"$indent/** ", s"\n$indent  * ", s"\n$indent  */\n")
    }
  }

  /** The parameters of `op` in signature order: path, then required, then those with a default. */
  def arguments(op: Operation, streaming: Streaming): List[Argument] = {
    val body = op.body.toList.flatMap {
      case RequestBody.Json(name, tpe) => List(Argument(name, tpe.render, None))
      case RequestBody.Text(name) => List(Argument(name, "String", None))
      case RequestBody.Bytes(name, _) => List(Argument(name, TypeRef.Bytes.render, None))
      case RequestBody.Stream(name, _) => List(Argument(name, streaming.binary, None))
      case RequestBody.Form(fields, _) => fields.map(f => Argument(f.name, f.tpe.render, f.default))
    }
    val (path, others) = op.params.partition(_.location == ParamLocation.Path)
    val rest = body ++ others.map(p => Argument(p.name, p.tpe.render, p.default))
    path.map(p => Argument(p.name, p.tpe.render, None)) ++ rest.filter(_.default.isEmpty) ++
      rest.filter(_.default.isDefined)
  }

  def resultType(op: Operation, streaming: Streaming): String = op.result match {
    case ResultBody.Json(tpe) => tpe.render
    case ResultBody.Text => "String"
    case ResultBody.Bytes => TypeRef.Bytes.render
    case ResultBody.Empty => "Unit"
    case ResultBody.Stream => streaming.binary
    case ResultBody.Events => streaming.events
  }

  /** `def name(params): effect[Result]`. */
  def signature(op: Operation, withDefaults: Boolean, streaming: Streaming, effect: String = "F"): String = {
    val params = arguments(op, streaming).map(_.declaration(withDefaults)).mkString(", ")
    s"def ${op.name.value}($params): $effect[${resultType(op, streaming)}]"
  }

  /** The imports the stream and event types of `operations` need. */
  def streamImports(operations: List[Operation], streaming: Streaming): List[String] = {
    val streams = operations.exists(op => op.streamsRequest || op.result == ResultBody.Stream)
    (if (streams) streaming.binaryImports else Nil) ++
      (if (operations.exists(_.result == ResultBody.Events)) streaming.eventImports else Nil)
  }

  /** The imports `types` need: their models and fully qualified names. */
  def importsOf(types: Iterable[TypeRef], model: ClientModel, packages: Packages): Set[String] = {
    val known = model.modelNames + TypeRef.RawJson
    types.flatMap(t => t.names.filter(known).map(packages.models.member) ++ t.qualifiedNames).toSet
  }
}
