package io.okapi.openapi

import io.circe.Printer
import io.circe.syntax.*
import scala.collection.immutable.ListMap
import sttp.apispec.{ ExtensionValue, SecurityScheme }
import sttp.apispec.openapi.{ Components, OpenAPI, Operation, PathItem }
import sttp.apispec.openapi.circe.*
import sttp.apispec.openapi.circe.yaml.*
import sttp.model.Method
import sttp.tapir.{ AnyEndpoint, EndpointInput, EndpointIO, EndpointOutput, EndpointTransput }
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/** OpenAPI documents and Swagger UI for a list of Tapir endpoints, in any effect. `customise` is applied to the
  * generated document, e.g. [[OkapiDocs.withBearerAuth]].
  *
  * An endpoint's name is its operation id; a name shared by several endpoints is prefixed with the endpoint's first tag
  * (`list` in `Books` → `booksList`), and an id still shared after that gets `2`, `3`, ... appended in document order,
  * before `customise` runs. Unnamed endpoints get Tapir's path-based id.
  *
  * An operation with a streamed request or response body carries Tapir codegen's `x-tapir-codegen-directives`
  * (`force-req-body-streaming`, `force-resp-body-streaming`).
  */
object OkapiDocs {

  type Customise = OpenAPI => OpenAPI

  /** The OpenAPI 3 document for `endpoints`. */
  def openApi(endpoints: List[AnyEndpoint], title: String, version: String, customise: Customise = identity): OpenAPI =
    standard(endpoints).andThen(customise)(OpenAPIDocsInterpreter().toOpenAPI(endpoints, title, version))

  /** [[openApi]] as YAML. */
  def yaml(endpoints: List[AnyEndpoint], title: String, version: String, customise: Customise = identity): String =
    openApi(endpoints, title, version, customise).toYaml

  /** [[openApi]] as JSON, without null fields. */
  def json(endpoints: List[AnyEndpoint], title: String, version: String, customise: Customise = identity): String =
    Printer.spaces2.copy(dropNullValues = true).print(openApi(endpoints, title, version, customise).asJson)

  /** Swagger UI under `/docs`, serving the document as `/docs/docs.yaml`. */
  def swagger[F[_]](
    endpoints: List[AnyEndpoint],
    title: String,
    version: String,
    customise: Customise = identity,
  ): List[ServerEndpoint[Any, F]] =
    SwaggerInterpreter(customiseDocsModel = standard(endpoints).andThen(customise))
      .fromEndpoints[F](endpoints, title, version)

  /** Declares a bearer security scheme named `name` and requires it on every operation. */
  def withBearerAuth(name: String = "bearerAuth", bearerFormat: Option[String] = Some("JWT")): Customise = { api =>
    val scheme = SecurityScheme(`type` = "http", scheme = Some("bearer"), bearerFormat = bearerFormat)
    val components = api.components.getOrElse(Components())
    api.copy(
      components = Some(components.copy(securitySchemes = components.securitySchemes + (name -> Right(scheme)))),
      security = List(ListMap(name -> Vector.empty)),
    )
  }

  private val Word = "[A-Za-z0-9]+".r

  private val Directives = "x-tapir-codegen-directives"

  private def standard(endpoints: List[AnyEndpoint]): Customise = markStreaming(endpoints).andThen(uniqueOperationIds)

  /** Marks the operations of `endpoints` whose request or response body is a stream. */
  private def markStreaming(endpoints: List[AnyEndpoint]): Customise = {
    val directives = endpoints.flatMap { e =>
      val streamed = List(
        Option.when(hasStream(e.input))("force-req-body-streaming"),
        Option.when(hasStream(e.output))("force-resp-body-streaming"),
      ).flatten
      Option.when(streamed.nonEmpty) {
        val method = e.method.getOrElse(Method.GET).method.toLowerCase
        val path =
          e.showPathTemplate(showQueryParam = None, includeAuth = false, showNoPathAs = "/", showPathsAs = None)
        (method, path) -> streamed
      }
    }.toMap
    api =>
      mapOperations(api) { (path, method, operation) =>
        directives.get((method, path)).fold(operation) { streamed =>
          val value = ExtensionValue(streamed.map(d => s""""$d"""").mkString("[", ",", "]"))
          operation.copy(extensions = operation.extensions + (Directives -> value))
        }
      }
  }

  private def hasStream(transput: EndpointTransput[?]): Boolean = transput match {
    case _: EndpointIO.StreamBodyWrapper[?, ?] => true
    case p: EndpointInput.Pair[?, ?, ?] => hasStream(p.left) || hasStream(p.right)
    case p: EndpointIO.Pair[?, ?, ?] => hasStream(p.left) || hasStream(p.right)
    case p: EndpointOutput.Pair[?, ?, ?] => hasStream(p.left) || hasStream(p.right)
    case m: EndpointInput.MappedPair[?, ?, ?, ?] => hasStream(m.input)
    case m: EndpointIO.MappedPair[?, ?, ?, ?] => hasStream(m.io)
    case m: EndpointOutput.MappedPair[?, ?, ?, ?] => hasStream(m.output)
    case o: EndpointOutput.OneOf[?, ?] => o.variants.exists(v => hasStream(v.output))
    case _ => false
  }

  /** Prefixes each operation id shared by several operations with the operation's first tag, then appends `2`, `3`, ...
    * to every later use of an id still shared.
    */
  private val uniqueOperationIds: Customise = { api =>
    val ops = operations(api)
    val shared = ops
      .flatMap(_._3.operationId)
      .groupBy(identity)
      .collect {
        case (id, uses) if uses.size > 1 => id
      }
      .toSet
    val prefixed = ops.map { (path, method, operation) =>
      (path, method) -> operation.operationId.map(id => if shared.contains(id) then tagPrefixed(id, operation) else id)
    }
    val taken = prefixed.flatMap(_._2).toSet
    val unique = prefixed
      .foldLeft((Set.empty[String], Map.empty[(String, String), String])) {
        case ((used, ids), (key, Some(id))) =>
          val chosen = {
            if !used.contains(id) then id
            else Iterator.from(2).map(n => s"$id$n").filterNot(c => taken.contains(c) || used.contains(c)).next()
          }
          (used + chosen, ids + (key -> chosen))
        case (state, (_, None)) => state
      }
      ._2
    mapOperations(api)((path, method, operation) => operation.copy(operationId = unique.get((path, method))))
  }

  /** `id` prefixed with `operation`'s first tag in camel case; `id` if that tag has no letters or digits. */
  private def tagPrefixed(id: String, operation: Operation): String = {
    operation.tags.headOption.fold(List.empty[String])(Word.findAllIn(_).toList) match {
      case first :: rest => first.head.toLower.toString + first.tail + rest.map(_.capitalize).mkString + id.capitalize
      case Nil => id
    }
  }

  /** `api` with `f(path, method, operation)` applied to every operation; `method` is lower case. */
  private def mapOperations(api: OpenAPI)(f: (String, String, Operation) => Operation): OpenAPI = {
    def item(path: String, i: PathItem): PathItem = {
      def op(method: String, o: Option[Operation]) = o.map(f(path, method, _))
      i.copy(
        get = op("get", i.get),
        put = op("put", i.put),
        post = op("post", i.post),
        delete = op("delete", i.delete),
        options = op("options", i.options),
        head = op("head", i.head),
        patch = op("patch", i.patch),
        trace = op("trace", i.trace),
      )
    }
    api.copy(paths = api.paths.copy(pathItems = api.paths.pathItems.map((path, i) => path -> item(path, i))))
  }

  /** Every operation of `api` with its path and lower-case method, in document order. */
  private def operations(api: OpenAPI): List[(String, String, Operation)] = {
    api.paths.pathItems.toList.flatMap { (path, item) =>
      List(
        "get" -> item.get,
        "put" -> item.put,
        "post" -> item.post,
        "delete" -> item.delete,
        "options" -> item.options,
        "head" -> item.head,
        "patch" -> item.patch,
        "trace" -> item.trace,
      ).collect { case (method, Some(operation)) => (path, method, operation) }
    }
  }
}
