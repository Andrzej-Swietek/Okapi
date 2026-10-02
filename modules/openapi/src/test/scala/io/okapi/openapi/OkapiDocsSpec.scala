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
      test("an endpoint's name is its operation id; a shared name is prefixed with the endpoint's tag") {
        val named = List(
          endpoint.get.in("books").name("list").tag("Books"),
          endpoint.get.in("users").name("list").tag("User accounts"),
          endpoint.get.in("stats").name("stats").tag("Admin"),
          endpoint.get.in("health"),
        )
        val ids =
          OkapiDocs.openApi(named, "API", "1").paths.pathItems.values.flatMap(_.get).flatMap(_.operationId).toSet
        assertTrue(ids == Set("booksList", "userAccountsList", "stats", "getHealth"))
      },
      test("operations with a streamed body carry Tapir codegen's streaming directives") {
        import sttp.capabilities.Streams
        object Bytes extends Streams[Bytes.type] {
          type BinaryStream = Iterator[Byte]
          type Pipe[A, B] = Iterator[A] => Iterator[B]
        }
        val streamed = List(
          endpoint.post.in("upload").in(streamBinaryBody(Bytes)(CodecFormat.OctetStream())),
          endpoint.get.in("download").out(streamBinaryBody(Bytes)(CodecFormat.OctetStream())),
          endpoint.get.in("plain").out(byteArrayBody),
        )
        val yaml = OkapiDocs.yaml(streamed, "API", "1")
        assertTrue(
          yaml.contains("x-tapir-codegen-directives:\n      - force-req-body-streaming"),
          yaml.contains("x-tapir-codegen-directives:\n      - force-resp-body-streaming"),
          "x-tapir-codegen-directives".r.findAllIn(yaml).size == 2,
        )
      },
      test("swagger serves the UI and the document for any effect") {
        val swagger = OkapiDocs.swagger[scala.util.Try](endpoints, "Books", "1")
        assertTrue(swagger.exists(_.endpoint.showPathTemplate(showQueryParam = None).startsWith("/docs")))
      },
    )
  }
}
