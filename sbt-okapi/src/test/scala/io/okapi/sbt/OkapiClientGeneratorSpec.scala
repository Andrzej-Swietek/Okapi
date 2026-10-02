package io.okapi.sbt

import zio.test._

import java.nio.file.Files

import _root_.sbt.io.IO

object OkapiClientGeneratorSpec extends ZIOSpecDefault {

  private val document = {
    """openapi: 3.1.0
      |info:
      |  title: Books
      |  version: '1'
      |paths:
      |  /api/books/{id}:
      |    get:
      |      operationId: getBook
      |      parameters:
      |      - name: id
      |        in: path
      |        required: true
      |        schema:
      |          type: integer
      |          format: int32
      |      responses:
      |        '200':
      |          description: ''
      |          content:
      |            application/json:
      |              schema:
      |                $ref: '#/components/schemas/Book'
      |components:
      |  schemas:
      |    Book:
      |      title: Book
      |      type: object
      |      required:
      |      - id
      |      - title
      |      properties:
      |        id:
      |          type: integer
      |          format: int32
      |        title:
      |          type: string
      |""".stripMargin
  }

  def spec = {
    test("writes the endpoints, the models and a build.sbt for the client module") {
      val directory = Files.createTempDirectory("okapi-client").toFile
      val files = OkapiClientGenerator.generate(
        document,
        "books.client",
        "BooksEndpoints",
        "books-client",
        "3.6.4",
        "fs2",
        directory,
      )
      val sources = files.filter(_.getName.endsWith(".scala")).map(IO.read(_)).mkString
      val build = IO.read(new java.io.File(directory, "build.sbt"))
      assertTrue(
        files.exists(_.getPath.endsWith("books/client/BooksEndpoints.scala")),
        sources.contains("lazy val getBook"),
        sources.contains("case class Book ("),
        build.contains("name := \"books-client\""),
        build.contains("tapir-sttp-client4"),
      )
    }
  }
}
