package io.okapi.codegen.render

import scala.collection.mutable
import io.okapi.codegen.*
import Source.{ codecMaker, doc, file, importsOf, signature, streamImports, Jsoniter }
import StreamingSyntax.*

/** Renders the sttp client4 implementation: a `Sttp<trait>` class per group over one shared `SttpTransport`, the root
  * `Sttp<api>` taking the backend, and the `ApiException` a non-2xx response fails with.
  */
private[codegen] object ImplRenderer {

  private val Codec = s"$Jsoniter.core.JsonValueCodec"
  private val Maker = s"$Jsoniter.macros.JsonCodecMaker"
  private val Config = s"$Jsoniter.macros.CodecMakerConfig"

  /** `Sttp<trait>`, the class implementing the trait `traitName`. */
  private def implName(traitName: Identifier): Identifier = Identifier.tpeOrConverted("Sttp" + traitName.bare)

  def render(model: ClientModel, settings: Settings): List[SourceFile] = {
    val packages = settings.packages
    val root = implName(settings.api)
    val impls = model.groups match {
      case List(single) if single.traitName == settings.api =>
        List(SourceFile(packages.impl.file(root.bare), groupFile(single, model, settings, root, standalone = true)))
      case groups =>
        groups.map { g =>
          val name = implName(g.traitName)
          SourceFile(packages.impl.file(name.bare), groupFile(g, model, settings, name, standalone = false))
        } :+ SourceFile(packages.impl.file(root.bare), rootFile(groups, settings, root))
    }
    impls ++ List(
      SourceFile(packages.impl.file("SttpTransport"), transport(model, settings)),
      SourceFile(packages.root.file("ApiException"), apiException(model, packages)),
    )
  }

  private def constructor(streaming: Streaming) =
    s"(backend: ${streaming.backend}, baseUri: Uri, headers: Seq[Header] = Nil)"

  private def clientDoc(settings: Settings) =
    s"The ${settings.title} client over an sttp backend; every request goes to `baseUri` with `headers`."

  private val TransportField = "  private val transport = SttpTransport(backend, baseUri, headers)\n"

  private def rootFile(groups: List[Group], settings: Settings, root: Identifier): String = {
    val streaming = settings.streaming
    val accessors = groups.map { g =>
      s"  override val ${g.accessor.value}: ${g.traitName.value}[${streaming.effect}] = " +
        s"${implName(g.traitName).value}(transport)"
    }
    val traits = settings.api.bare :: groups.map(_.traitName.bare)
    file(
      settings.packages.impl,
      traits
        .map(settings.packages.root.member) ++ streaming.backendImports ++ List("sttp.model.Header", "sttp.model.Uri"),
      List(
        doc(clientDoc(settings)) +
          s"final class ${root.value}${streaming.typeParams}${constructor(streaming)} " +
          s"extends ${settings.api.value}[${streaming.effect}] {\n" + TransportField + "\n" +
          accessors.mkString("\n\n") + "\n}"
      ),
    )
  }

  private def groupFile(group: Group, model: ClientModel, settings: Settings, name: Identifier, standalone: Boolean) = {
    val (packages, streaming) = (settings.packages, settings.streaming)
    val imports = mutable.Set[String](packages.root.member(group.traitName.bare), "sttp.model.Uri.UriContext")
    imports ++= importsOf(group.operations.flatMap(_.types), model, packages)
    imports ++= streamImports(group.operations, streaming) ++ streaming.effectImports
    val members = mutable.SortedSet[String]("request")
    val codecs = localCodecs(group.operations.flatMap(jsonTypes), model)
    if (codecs.nonEmpty) imports ++= List(Codec, Maker, Config)
    val methods = group.operations.map(op => method(op, model, streaming, imports, members))
    val head =
      if (standalone) {
        imports ++= streaming.backendImports ++ List("sttp.model.Header", "sttp.model.Uri")
        s"final class ${name.value}${streaming.typeParams}${constructor(streaming)} " +
          s"extends ${group.traitName.value}[${streaming.effect}] {\n" + TransportField
      }
      else {
        members += "baseUri"
        s"final class ${name.value}${streaming.typeParams}(transport: SttpTransport${streaming.typeArgs}) " +
          s"extends ${group.traitName.value}[${streaming.effect}] {\n"
      }
    val givens = codecs.map { (codec, tpe) =>
      s"  private given $codec: JsonValueCodec[${tpe.render}] = ${codecMaker(model.isRecursive(tpe))}"
    }
    val body = (List(head + s"  import transport.{ ${members.mkString(", ")} }\n") ++
      (if (givens.isEmpty) Nil else List(givens.mkString("", "\n", "\n"))) ++ methods).mkString("\n") + "}"
    val comment =
      if (standalone) clientDoc(settings)
      else s"The sttp implementation of [[${group.traitName.value}]]."
    file(packages.impl, imports, List(doc(comment) + body))
  }

  private def jsonTypes(op: Operation): List[TypeRef] = {
    op.result.types ++ op.body.toList.flatMap {
      case RequestBody.Json(_, tpe) => List(tpe)
      case _ => Nil
    }
  }

  /** The JSON types without a codec in a model companion, with the name of the codec made for each. */
  private def localCodecs(types: List[TypeRef], model: ClientModel): List[(String, TypeRef)] = {
    types.distinct.filterNot(hasCompanionCodec(_, model)).foldLeft(List.empty[(String, TypeRef)]) { (done, tpe) =>
      val name = Identifier.unique(Identifier.term(tpe.render + "Codec"), done.map(d => Identifier.term(d._1)).toSet)
      done :+ (name.value -> tpe)
    }
  }

  private def hasCompanionCodec(tpe: TypeRef, model: ClientModel): Boolean = tpe match {
    case TypeRef.Named(TypeRef.RawJson) => true
    case TypeRef.Named(name) =>
      model.model(name).exists {
        case r: Model.Record => r.parents.isEmpty
        case _ => true
      }
    case _ => false
  }

  /** `x.toString`, or `x` when it is a `String` already. */
  private def text(value: String, tpe: TypeRef): String =
    if (tpe == TypeRef.Named("String")) value else s"$value.toString"

  /** A header value: a list's values joined by commas, as OpenAPI's `simple` style sends them. */
  private def headerValue(value: String, tpe: TypeRef): String = tpe match {
    case _: TypeRef.Seq => s"""$value.mkString(",")"""
    case other => text(value, other)
  }

  /** The method sending `op`; `members` collects the transport members it uses. */
  private def method(
    op: Operation,
    model: ClientModel,
    streaming: Streaming,
    imports: mutable.Set[String],
    members: mutable.Set[String],
  ): String = {
    val request = new StringBuilder(s"request.${op.method.sttp}(${uri(op)})")
    op.params.filter(_.location == ParamLocation.Header).foreach { p =>
      val (name, wire) = (p.name.value, p.wire.literal)
      request ++= ((p.tpe, p.default) match {
        case (TypeRef.Opt(of), Some(_)) =>
          imports += "sttp.model.Header"
          s".headers($name.map(v => Header($wire, ${headerValue("v", of)})).toSeq*)"
        case (seq: TypeRef.Seq, Some(_)) =>
          imports += "sttp.model.Header"
          s".headers(Option.when($name.nonEmpty)(Header($wire, ${headerValue(name, seq)})).toSeq*)"
        case (tpe, _) => s".header($wire, ${headerValue(name, tpe)})"
      })
    }
    op.body.foreach {
      case RequestBody.Json(name, _) =>
        imports ++= List(s"$Jsoniter.core.writeToArray", "sttp.model.MediaType")
        request ++= s".body(writeToArray(${name.value})).contentType(MediaType.ApplicationJson)"
      case RequestBody.Text(name) => request ++= s".body(${name.value})"
      case RequestBody.Bytes(name, contentType) =>
        request ++= s".body(${name.value}).contentType(${Literal(contentType)})"
      case RequestBody.Stream(name, contentType) =>
        if (streaming == Streaming.Disabled) request ++= s".body(${name.value})"
        else {
          members += "streams"
          request ++= s".streamBody(streams)(${name.value})"
        }
        request ++= s".contentType(${Literal(contentType)})"
      case RequestBody.Form(fields, multipart) =>
        val parts = fields.map(f => formField(f, multipart, model, imports))
        if (multipart) {
          imports += "sttp.client4.multipart"
          request ++= s".multipartBody(List(${parts.mkString(", ")}).flatten)"
        }
        else request ++= s""".body(List(${parts.mkString(", ")}).flatten, "utf-8")"""
    }
    val (send, response) = op.result match {
      case ResultBody.Json(tpe) => (s"transport.json[${tpe.render}]", "asBody")
      case ResultBody.Text => ("transport.text", "asBody")
      case ResultBody.Bytes => ("transport.bytes", "asBody")
      case ResultBody.Empty => ("transport.unit", "asBody")
      case ResultBody.Stream if streaming == Streaming.Disabled => ("transport.bytes", "asBody")
      case ResultBody.Stream => ("transport.send", "asStream")
      case ResultBody.Events => ("transport.send", "asEvents")
    }
    members += response
    val signatureText = signature(op, withDefaults = false, streaming, streaming.effect)
    s"  override $signatureText =\n    $send($request.response($response))\n"
  }

  /** `uri"$baseUri/books/$id?limit=$limit"`; sttp leaves out a `None` query value and repeats a list's. */
  private def uri(op: Operation): String = {
    def escape(text: String) = text.replace("$", "$$").replace("\"", "%22").replace("\\", "%5C")
    def splice(name: Identifier, braced: Boolean) =
      if (!braced && name.value == name.bare) "$" + name.value else s"$${${name.value}}"
    val path = op.path.map { segment =>
      val next = segment.parts.drop(1).map(Some(_)) :+ None
      segment.parts
        .zip(next)
        .map {
          case (PathPart.Literal(text), _) => escape(text)
          case (PathPart.Capture(param), following) => splice(param, braced = following.isDefined)
        }
        .mkString
    }
    val query = op.params
      .filter(_.location == ParamLocation.Query)
      .map(p => s"${escape(p.wire.value)}=${splice(p.name, braced = false)}")
    "uri\"$baseUri/" + path.mkString("/") + (if (query.isEmpty) "" else query.mkString("?", "&", "")) + "\""
  }

  /** A form field as the list of its parts (multipart) or name-value pairs. */
  private def formField(field: FormField, multipart: Boolean, model: ClientModel, imports: mutable.Set[String])
    : String = {
    val name = field.name.value
    val wire = field.wire.literal
    def asText(value: String, tpe: TypeRef): String = tpe match {
      case TypeRef.Named(n) if model.model(n).exists(!_.isInstanceOf[Model.StringEnum]) =>
        imports += s"$Jsoniter.core.writeToString"
        s"writeToString($value)"
      case _ => text(value, tpe)
    }
    def entry(value: String, tpe: TypeRef): String =
      if (!multipart) s"$wire -> ${asText(value, tpe)}"
      else if (field.binary) s"multipart($wire, $value).fileName($wire)"
      else s"multipart($wire, ${asText(value, tpe)})"
    field.tpe match {
      case TypeRef.Opt(of) => s"$name.toList.map(v => ${entry("v", of)})"
      case TypeRef.Seq(of, _) => s"$name.toList.map(v => ${entry("v", of)})"
      case tpe => s"List(${entry(name, tpe)})"
    }
  }

  private def transport(model: ClientModel, settings: Settings): String = {
    val streaming = settings.streaming
    val streamed = streaming != Streaming.Disabled
    val (streams, events) = (model.usesStreams && streamed, model.usesEvents)
    val effect = streaming.effect
    val imports = List(
      settings.packages.root.member("ApiException"),
      s"$Jsoniter.core.JsonValueCodec",
      s"$Jsoniter.core.readFromArray",
      "java.nio.charset.StandardCharsets.UTF_8",
      "sttp.client4.GenericRequest",
      "sttp.client4.PartialRequest",
      "sttp.client4.ResponseAs",
      "sttp.client4.asByteArray",
      "sttp.client4.basicRequest",
      "sttp.model.Header",
      "sttp.model.ResponseMetadata",
      "sttp.model.Uri",
    ) ++ streaming.backendImports ++
      (if (streamed) List("sttp.capabilities.Effect") else Nil) ++
      (if (streams || (events && streamed)) List("sttp.client4.StreamResponseAs", "sttp.client4.asStreamUnsafe")
       else Nil) ++
      (if (streams || (events && streamed)) streaming.binaryImports else Nil) ++
      (if (events) List("sttp.model.sse.ServerSentEvent") else Nil) ++
      (if (events && streamed) List(streaming.eventParserImport) else Nil)

    val streamMembers =
      if (!streams && !(events && streamed)) ""
      else
        s"""
           |  val streams: ${streaming.streamsType} = ${streaming.streamsValue}
           |
           |  /** The body of a 2xx response as a stream the caller must consume, else the [[ApiException]] it is. */
           |  val asStream: StreamResponseAs[Either[ApiException, ${streaming.binary}], ${streaming.streamsType}] =
           |    asStreamUnsafe(streams).mapWithMetadata(failure)
           |""".stripMargin
    val eventMembers =
      if (!events) ""
      else if (streamed)
        s"""
           |  /** The server-sent events of a 2xx response, else the [[ApiException]] the response is. */
           |  val asEvents: StreamResponseAs[Either[ApiException, ${streaming.events}], ${streaming.streamsType}] =
           |    asStream.map(_.map(${streaming.eventParser}))
           |""".stripMargin
      else
        """
          |  /** The server-sent events of a 2xx response, read whole, else the [[ApiException]] the response is. */
          |  val asEvents: ResponseAs[Either[ApiException, List[ServerSentEvent]]] = asBody.map(_.map(SttpTransport.events))
          |""".stripMargin
    val companion =
      if (events && !streamed)
        """
          |
          |object SttpTransport {
          |  private val Line = "\r?\n".r
          |  private val BlankLine = "\r?\n\r?\n".r
          |
          |  private def events(body: Array[Byte]): List[ServerSentEvent] =
          |    BlankLine.split(new String(body, UTF_8)).toList.filter(_.exists(!_.isWhitespace))
          |      .map(event => ServerSentEvent.parse(Line.split(event).toList))
          |}""".stripMargin
      else ""
    val body =
      s"""/** Sends the client's requests, each with `headers`; a non-2xx response fails the effect with [[ApiException]]. */
         |final class SttpTransport${streaming.typeParams}(backend: ${streaming.backend}, val baseUri: Uri, headers: Seq[Header]) {
         |
         |  /** A request the client sends. */
         |  type Sendable[A] = GenericRequest[Either[ApiException, A], ${streaming.capabilities}]
         |
         |  /** A request with the client's headers. */
         |  val request: PartialRequest[Either[String, String]] = basicRequest.headers(headers*)
         |
         |  /** The body of a 2xx response, else the [[ApiException]] the response is. */
         |  val asBody: ResponseAs[Either[ApiException, Array[Byte]]] = asByteArray.mapWithMetadata(failure)
         |$streamMembers$eventMembers
         |  def json[A: JsonValueCodec](request: Sendable[Array[Byte]]): $effect[A] = read(request)(readFromArray[A](_))
         |
         |  def text(request: Sendable[Array[Byte]]): $effect[String] = read(request)(new String(_, UTF_8))
         |
         |  def bytes(request: Sendable[Array[Byte]]): $effect[Array[Byte]] = read(request)(identity)
         |
         |  def unit(request: Sendable[Array[Byte]]): $effect[Unit] = read(request)(_ => ())
         |
         |  def send[A](request: Sendable[A]): $effect[A] = read(request)(identity)
         |
         |  private def read[A, B](request: Sendable[A])(f: A => B): $effect[B] = {
         |    val monad = backend.monad
         |    monad.flatMap(backend.send(request))(response => response.body.fold(monad.error, a => monad.eval(f(a))))
         |  }
         |
         |  private def failure[A](body: Either[String, A], meta: ResponseMetadata): Either[ApiException, A] =
         |    body.left.map(ApiException(meta.code.code, _))
         |}$companion""".stripMargin
    file(settings.packages.impl, imports, List(body))
  }

  private def apiException(model: ClientModel, packages: Packages): String = {
    val errorType = "ApiErrorResponse"
    val decoded = model.model(errorType).isDefined
    val error =
      if (decoded)
        s"""
           |
           |  /** The body as an [[$errorType]], when it is one. */
           |  def error: Option[$errorType] = Try(readFromString[$errorType](body)).toOption
           |""".stripMargin
      else ""
    file(
      packages.root,
      if (decoded) List(packages.models.member(errorType), s"$Jsoniter.core.readFromString", "scala.util.Try") else Nil,
      List(
        s"""/** A response with a non-2xx `status`. */
           |final case class ApiException(status: Int, body: String) extends RuntimeException(s"HTTP $$status: $$body")${
            if (decoded) s" {$error}" else ""
          }""".stripMargin
      ),
    )
  }
}
