package io.okapi.openapi

import zio.test.*

import sttp.capabilities.Streams
import sttp.tapir.*

object OkapiDocsSpec extends ZIOSpecDefault {

  private object Bytes extends Streams[Bytes.type] {
    type BinaryStream = Iterator[Byte]
    type Pipe[A, B] = Iterator[A] => Iterator[B]
  }

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
      test("operation ids still shared after the tag prefix get 2, 3, ... appended") {
        val clashing = List(
          endpoint.get.in("a").name("list").tag("Books"),
          endpoint.post.in("a").name("list").tag("Books"),
          endpoint.get.in("b").name("list").tag("Books!"),
          endpoint.get.in("c").name("list").tag("???"),
          endpoint.get.in("d").name("booksList"),
          endpoint.get.in("e").name("list"),
        )
        val ids = OkapiDocs
          .openApi(clashing, "API", "1")
          .paths
          .pathItems
          .values
          .toList
          .flatMap(item => item.get.toList ++ item.post.toList)
          .flatMap(_.operationId)
        assertTrue(
          ids.size == 6,
          ids.toSet == Set("booksList", "booksList2", "booksList3", "list", "booksList4", "list2"),
        )
      },
      test("operations with a streamed body carry Tapir codegen's streaming directives") {
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
      test("a streamed root-path or method-less endpoint carries the directive on its documented operation") {
        val api = OkapiDocs.openApi(
          List(
            endpoint.get.out(streamBinaryBody(Bytes)(CodecFormat.OctetStream())),
            endpoint.in("any").out(streamBinaryBody(Bytes)(CodecFormat.OctetStream())),
          ),
          "API",
          "1",
        )
        def marked(path: String) =
          api.paths.pathItems.get(path).flatMap(_.get).exists(_.extensions.contains("x-tapir-codegen-directives"))
        assertTrue(marked("/"), marked("/any"))
      },
      test("swagger serves the UI and the document for any effect") {
        val swagger = OkapiDocs.swagger[scala.util.Try](endpoints, "Books", "1")
        assertTrue(swagger.exists(_.endpoint.showPathTemplate(showQueryParam = None).startsWith("/docs")))
      },
    )
  }
}
