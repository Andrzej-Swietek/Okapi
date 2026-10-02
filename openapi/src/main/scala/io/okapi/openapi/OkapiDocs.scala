package io.okapi.openapi

import io.circe.Printer
import io.circe.syntax.*
import scala.collection.immutable.ListMap
import sttp.apispec.SecurityScheme
import sttp.apispec.openapi.{ Components, OpenAPI }
import sttp.apispec.openapi.circe.*
import sttp.apispec.openapi.circe.yaml.*
import sttp.tapir.AnyEndpoint
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/** OpenAPI documents and Swagger UI for a list of endpoints — Okapi's and any others — in any effect. `customise` is
  * applied to the generated document, e.g. [[OkapiDocs.withBearerAuth]].
  */
object OkapiDocs {

  type Customise = OpenAPI => OpenAPI

  def openApi(endpoints: List[AnyEndpoint], title: String, version: String, customise: Customise = identity): OpenAPI =
    customise(OpenAPIDocsInterpreter().toOpenAPI(endpoints, title, version))

  def yaml(endpoints: List[AnyEndpoint], title: String, version: String, customise: Customise = identity): String =
    openApi(endpoints, title, version, customise).toYaml

  def json(endpoints: List[AnyEndpoint], title: String, version: String, customise: Customise = identity): String =
    Printer.spaces2.copy(dropNullValues = true).print(openApi(endpoints, title, version, customise).asJson)

  /** Swagger UI under `/docs`, serving the document as `/docs/docs.yaml`. */
  def swagger[F[_]](
    endpoints: List[AnyEndpoint],
    title: String,
    version: String,
    customise: Customise = identity,
  ): List[ServerEndpoint[Any, F]] =
    SwaggerInterpreter(customiseDocsModel = customise).fromEndpoints[F](endpoints, title, version)

  /** Declares a bearer security scheme named `name` and requires it on every operation. */
  def withBearerAuth(name: String = "bearerAuth", bearerFormat: Option[String] = Some("JWT")): Customise = { api =>
    val scheme = SecurityScheme(`type` = "http", scheme = Some("bearer"), bearerFormat = bearerFormat)
    val components = api.components.getOrElse(Components())
    api.copy(
      components = Some(components.copy(securitySchemes = components.securitySchemes + (name -> Right(scheme)))),
      security = List(ListMap(name -> Vector.empty)),
    )
  }
}
