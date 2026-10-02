package io.okapi.codegen

import zio.test.*

import java.io.File
import java.nio.file.Files

import scala.jdk.CollectionConverters.*

object OkapiCodegenSpec extends ZIOSpecDefault {

  val document: String = {
    """openapi: 3.1.0
      |info:
      |  title: Books
      |  version: '1'
      |paths:
      |  /api/books:
      |    get:
      |      operationId: listBooks
      |      summary: Lists the books
      |      tags: [Books]
      |      parameters:
      |      - name: genre
      |        in: query
      |        required: false
      |        schema:
      |          $ref: '#/components/schemas/Genre'
      |      - name: X-Request-Id
      |        in: header
      |        required: true
      |        schema:
      |          type: string
      |      responses:
      |        '200':
      |          description: ''
      |          content:
      |            application/json:
      |              schema:
      |                type: array
      |                items:
      |                  $ref: '#/components/schemas/Book'
      |    post:
      |      operationId: createBook
      |      tags: [Books]
      |      requestBody:
      |        required: true
      |        content:
      |          application/json:
      |            schema:
      |              $ref: '#/components/schemas/Book'
      |      responses:
      |        '201':
      |          description: ''
      |          content:
      |            application/json:
      |              schema:
      |                $ref: '#/components/schemas/Book'
      |  /api/books/{id}:
      |    delete:
      |      operationId: booksDeleteBook
      |      tags: [Books]
      |      parameters:
      |      - name: id
      |        in: path
      |        required: true
      |        schema:
      |          type: integer
      |          format: int32
      |      responses:
      |        '204':
      |          description: ''
      |  /api/books/{id}/content:
      |    get:
      |      operationId: downloadContent
      |      tags: [Books]
      |      x-tapir-codegen-directives: [force-resp-body-streaming]
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
      |            application/octet-stream:
      |              schema:
      |                type: string
      |                format: binary
      |    put:
      |      operationId: uploadContent
      |      tags: [Books]
      |      x-tapir-codegen-directives: [force-req-body-streaming]
      |      parameters:
      |      - name: id
      |        in: path
      |        required: true
      |        schema:
      |          type: integer
      |          format: int32
      |      requestBody:
      |        required: true
      |        content:
      |          application/octet-stream:
      |            schema:
      |              type: string
      |              format: binary
      |      responses:
      |        '204':
      |          description: ''
      |  /api/books/events:
      |    get:
      |      operationId: bookEvents
      |      tags: [Books]
      |      responses:
      |        '200':
      |          description: ''
      |          content:
      |            text/event-stream:
      |              schema:
      |                type: string
      |  /api/books/{id}/cover:
      |    post:
      |      operationId: uploadCover
      |      tags: [CoverController]
      |      parameters:
      |      - name: id
      |        in: path
      |        required: true
      |        schema:
      |          type: integer
      |          format: int32
      |      requestBody:
      |        content:
      |          multipart/form-data:
      |            schema:
      |              type: object
      |              required: [file]
      |              properties:
      |                file:
      |                  type: string
      |                  format: binary
      |                caption:
      |                  type: string
      |      responses:
      |        '200':
      |          description: ''
      |          content:
      |            text/plain:
      |              schema:
      |                type: string
      |    get:
      |      operationId: cover
      |      tags: [CoverController]
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
      |            image/png:
      |              schema:
      |                type: string
      |                format: binary
      |components:
      |  schemas:
      |    ApiErrorResponse:
      |      type: object
      |      required: [code, message]
      |      properties:
      |        code:
      |          type: integer
      |          format: int32
      |        message:
      |          type: string
      |    Book:
      |      type: object
      |      required: [id, title, shape]
      |      properties:
      |        id:
      |          type: integer
      |          format: int32
      |        title:
      |          type: string
      |        page_count:
      |          type: integer
      |          format: int32
      |        tags:
      |          type: array
      |          items:
      |            type: string
      |        shape:
      |          $ref: '#/components/schemas/Shape'
      |        sequel:
      |          $ref: '#/components/schemas/Book'
      |    Genre:
      |      type: string
      |      enum: [sci-fi, drama]
      |    Shape:
      |      oneOf:
      |      - $ref: '#/components/schemas/Circle'
      |      - $ref: '#/components/schemas/Dot'
      |      discriminator:
      |        propertyName: type
      |        mapping:
      |          Circle: '#/components/schemas/Circle'
      |          Dot: '#/components/schemas/Dot'
      |    Circle:
      |      type: object
      |      required: [radius, type]
      |      properties:
      |        radius:
      |          type: number
      |          format: double
      |        type:
      |          type: string
      |    Dot:
      |      type: object
      |      required: [type]
      |      properties:
      |        type:
      |          type: string
      |""".stripMargin
  }

  def settings(
    directory: File,
    split: Boolean = true,
    separate: Boolean = true,
    streaming: Streaming = Streaming.Disabled,
    pkg: String = "books.client",
  ): Settings = {
    val root = PackageName(pkg).toOption.get
    Settings(
      directory = directory,
      sourceDirectory = new File(directory, "src/main/scala"),
      packages = Packages(root, root.sub("models"), root.sub("impl")),
      api = Identifier.tpe("Library"),
      title = "Books",
      module = Module("books-client", Some("com.example"), None, "3.6.4"),
      buildFile = BuildFile.IfMissing,
      grouping = if (split) Grouping.ByController("Routes") else Grouping.Single,
      streaming = streaming,
      separateModels = separate,
      clean = true,
    )
  }

  /** The generated sources by path relative to the source root. */
  def generated(
    split: Boolean = true,
    separate: Boolean = true,
    streaming: Streaming = Streaming.Disabled,
  ): Map[String, String] = {
    val directory = Files.createTempDirectory("okapi-client").toFile
    val s = settings(directory, split, separate, streaming)
    val files = OkapiCodegen.generate(document, s).fold(e => throw new AssertionError(e), identity)
    files
      .filter(_.getName.endsWith(".scala"))
      .map(f => s.sourceDirectory.toPath.relativize(f.toPath).toString -> Files.readString(f.toPath))
      .toMap
  }

  def spec = {
    suite("OkapiCodegenSpec")(
      test("a tagless trait per controller, reached from the root trait") {
        val sources = generated()
        val books = sources("books/client/BooksRoutes.scala")
        assertTrue(
          sources("books/client/Library.scala").contains("def books: BooksRoutes[F]"),
          sources("books/client/Library.scala").contains("def cover: CoverRoutes[F]"),
          books.contains("/** Lists the books */"),
          books.contains("def listBooks(xRequestId: String, genre: Option[Genre] = None): F[List[Book]]"),
          books.contains("def createBook(book: Book): F[Book]"),
          books.contains("def deleteBook(id: Int): F[Unit]"),
          books.contains("import books.client.models.{ Book, Genre }"),
        )
      },
      test("form, text and binary bodies") {
        val cover = generated()("books/client/CoverRoutes.scala")
        assertTrue(
          cover.contains("def uploadCover(id: Int, file: Array[Byte], caption: Option[String] = None): F[String]"),
          cover.contains("def cover(id: Int): F[Array[Byte]]"),
        )
      },
      test("models: case classes with codecs, string enums, sealed traits with their cases in one file") {
        val sources = generated()
        val book = sources("books/client/models/Book.scala")
        val shape = sources("books/client/models/Shape.scala")
        val genre = sources("books/client/models/Genre.scala")
        assertTrue(
          book.contains("@named(\"page_count\") pageCount: Option[Int] = None"),
          book.contains("tags: List[String] = Nil"),
          book.contains(
            "given JsonValueCodec[Book] = " +
              "JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false).withAllowRecursiveTypes(true))"
          ),
          shape.contains("sealed trait Shape"),
          shape.contains("withDiscriminatorFieldName(Some(\"type\"))"),
          shape.contains("final case class Circle(radius: Double) extends Shape"),
          shape.contains("case object Dot extends Shape"),
          !shape.contains("given JsonValueCodec[Circle]"),
          !sources.contains("books/client/models/Circle.scala"),
          genre.contains("case SciFi extends Genre(\"sci-fi\")"),
        )
      },
      test("the sttp implementation sends each operation's request") {
        val sources = generated()
        val books = sources("books/client/impl/SttpBooksRoutes.scala")
        val cover = sources("books/client/impl/SttpCoverRoutes.scala")
        assertTrue(
          books.contains(
            """request.get(uri"$baseUri/api/books?genre=$genre").header("X-Request-Id", xRequestId).response(asBody)"""
          ),
          books.contains(
            "private given listBookCodec: JsonValueCodec[List[Book]] = " +
              "JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))"
          ),
          books.contains("transport.unit(request.delete(uri\"$baseUri/api/books/$id\").response(asBody))"),
          cover.contains("multipart(\"file\", file).fileName(\"file\")"),
          cover.contains("caption.toList.map(v => multipart(\"caption\", v))"),
          sources("books/client/impl/SttpLibrary.scala")
            .contains("override val books: BooksRoutes[F] = SttpBooksRoutes(transport)"),
          sources("books/client/ApiException.scala").contains("def error: Option[ApiErrorResponse]"),
        )
      },
      test("streamed bodies and server-sent events, per streaming mode") {
        val fs2 = generated(streaming = Streaming.Fs2)
        val zio = generated(streaming = Streaming.Zio)
        val none = generated()
        assertTrue(
          fs2("books/client/BooksRoutes.scala").contains("def downloadContent(id: Int): F[Stream[F, Byte]]"),
          fs2("books/client/BooksRoutes.scala").contains("def uploadContent(id: Int, body: Stream[F, Byte]): F[Unit]"),
          fs2("books/client/BooksRoutes.scala").contains("def bookEvents(): F[Stream[F, ServerSentEvent]]"),
          fs2("books/client/impl/SttpBooksRoutes.scala").contains(".streamBody(streams)(body)"),
          fs2("books/client/impl/SttpLibrary.scala").contains("(backend: StreamBackend[F, Fs2Streams[F]]"),
          fs2("books/client/impl/SttpTransport.scala").contains("asStream.map(_.map(Fs2ServerSentEvents.parse[F]))"),
          zio("books/client/BooksRoutes.scala")
            .contains("def downloadContent(id: Int): F[ZStream[Any, Throwable, Byte]]"),
          zio("books/client/impl/SttpBooksRoutes.scala").contains("extends BooksRoutes[Task]"),
          zio("books/client/impl/SttpLibrary.scala").contains("(backend: StreamBackend[Task, ZioStreams]"),
          none("books/client/BooksRoutes.scala").contains("def downloadContent(id: Int): F[Array[Byte]]"),
          none("books/client/BooksRoutes.scala").contains("def bookEvents(): F[List[ServerSentEvent]]"),
        )
      },
      test("names from the document avoid Scala, sttp and generated names; discriminator values follow the mapping") {
        val hostile = scala.util.Using.resource(scala.io.Source.fromResource("hostile.yaml"))(_.mkString)
        val directory = Files.createTempDirectory("okapi-client").toFile
        val s = settings(directory, pkg = "hostile.client").copy(api = Identifier.tpe("Hostile"))
        val sources = OkapiCodegen
          .generate(hostile, s)
          .fold(e => throw new AssertionError(e), identity)
          .filter(_.getName.endsWith(".scala"))
          .map(f => f.getName -> Files.readString(f.toPath))
          .toMap
        assertTrue(
          Set("ListModel.scala", "UriModel.scala", "RequestModel.scala", "StreamModel.scala").subsetOf(sources.keySet),
          !sources.contains("List.scala"),
          sources("Shape.scala").contains("@named(\"circle\")"),
          sources("UserAccountsRoutes.scala").contains("def listItems2("),
          sources("RequestModel.scala").contains("@named(\"class\") `class`: String") ||
          sources("RequestModel.scala").contains("`class`: String"),
          sources("Collide.scala").contains("@named(\"@type\") `type`: Option[String] = None, @named(\"type\") type2"),
          sources("SttpHostile.scala")
            .contains("override val _3dModels: _3dModelsRoutes[F] = Sttp_3dModelsRoutes(transport)"),
          sources.contains("Sttp_3dModelsRoutes.scala"),
          sources("SttpFilesRoutes.scala").contains("uri\"$baseUri/api/files/${name}.json\""),
          sources("SttpFilesRoutes.scala").contains(""".header("X-Ids", xIds.mkString(","))"""),
          sources("Shape.scala").contains("final case class Point() extends Shape"),
          sources("Entry.scala").contains("withAllowRecursiveTypes(true)"),
          !sources.contains("Odd.scala"),
        )
      },
      test("a unique name stays a valid identifier when the wanted one is a keyword") {
        val taken = Set(Identifier.term("type"))
        assertTrue(
          Identifier.unique(Identifier.term("type"), taken).value == "type2",
          Identifier.unique(Identifier.term("type"), taken, suffix = "Client").value == "typeClient",
        )
      },
      test("without the split, every operation is on the root trait; without separate models, one Models.scala") {
        val sources = generated(split = false, separate = false)
        assertTrue(
          sources("books/client/Library.scala").contains("def uploadCover("),
          sources("books/client/impl/SttpLibrary.scala")
            .contains("(backend: Backend[F], baseUri: Uri, headers: Seq[Header] = Nil)"),
          sources("books/client/impl/SttpLibrary.scala").contains("import sttp.model.{ Header, MediaType, Uri }"),
          sources.keySet.filter(_.contains("/models/")) == Set("books/client/models/Models.scala"),
        )
      },
      test("build.sbt is written when missing and kept afterwards") {
        val directory = Files.createTempDirectory("okapi-client").toFile
        val build = new File(directory, "build.sbt")
        val first = OkapiCodegen.generate(document, settings(directory))
        Files.writeString(build.toPath, "// edited")
        val second = OkapiCodegen.generate(document, settings(directory))
        assertTrue(
          first.exists(_.contains(build)),
          second.exists(!_.contains(build)),
          Files.readString(build.toPath) == "// edited",
        )
      },
      test("settings load from okapi.client.* properties") {
        val file = Files.createTempFile("okapi", ".properties")
        Files.write(
          file,
          List(
            "okapi.client.directory=/tmp/x",
            "okapi.client.package=books.client",
            "okapi.client.api=Library",
            "okapi.client.name=books-client",
            "okapi.client.scalaVersion=3.6.4",
            "okapi.client.buildFile=never",
            "okapi.client.implPackage=sttp4",
            "okapi.client.streaming=zio",
          ).asJava,
        )
        val loaded = Settings.load(file.toFile)
        assertTrue(
          loaded.map(_.buildFile) == Right(BuildFile.Never),
          loaded.map(_.streaming) == Right(Streaming.Zio),
          loaded.map(_.packages.impl.value) == Right("books.client.sttp4"),
          loaded.map(_.sourceDirectory) == Right(new File("/tmp/x/src/main/scala")),
        )
      },
    )
  }
}
