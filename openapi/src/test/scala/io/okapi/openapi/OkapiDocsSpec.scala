package io.okapi.openapi

import zio.test.*

import sttp.tapir.*

object OkapiDocsSpec extends ZIOSpecDefault {

  private val endpoints = List(endpoint.get.in("books" / path[Int]("id")).out(stringBody))

  override def spec = {
    suite("OkapiDocs")(
      test("JSON and YAML render the same document") {
        val json = OkapiDocs.json(endpoints, "Books", "1")
        val yaml = OkapiDocs.yaml(endpoints, "Books", "1")
        assertTrue(json.contains(""""/books/{id}""""), yaml.contains("/books/{id}:"), !json.contains("null"))
      },
      test("withBearerAuth declares the scheme and requires it on every operation") {
        val json = OkapiDocs.json(endpoints, "Books", "1", OkapiDocs.withBearerAuth())
        assertTrue(
          json.contains(""""bearerAuth" : {"""),
          json.contains(""""scheme" : "bearer""""),
          json.contains(""""security" : ["""),
        )
      },
      test("swagger serves the UI and the document for any effect") {
        val swagger = OkapiDocs.swagger[scala.util.Try](endpoints, "Books", "1")
        assertTrue(swagger.exists(_.endpoint.showPathTemplate(showQueryParam = None).startsWith("/docs")))
      },
    )
  }
}
