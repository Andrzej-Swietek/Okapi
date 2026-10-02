package io.okapi.core

import zio.test.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }
import io.okapi.core.annotations.{
  Consumes,
  Controller,
  Delete,
  Get,
  Header,
  Path,
  Post,
  Produces,
  Query,
  RequestBody,
  Status,
}
import io.okapi.core.http.{ ApiResponse, FileResponse }
import io.okapi.core.json.JsoniterCodec
import scala.util.{ Success, Try }
import sttp.model.StatusCode
import sttp.monad.TryMonad
import sttp.tapir.{ DecodeResult, EndpointInput, EndpointIO, EndpointOutput, EndpointTransput }
import sttp.tapir.generic.auto.*
import sttp.tapir.server.ServerEndpoint

/** okapi-core features independent of any backend, checked through Tapir's endpoint descriptions and server logic. */
object CoreFeaturesSpec extends ZIOSpecDefault {

  /** Carries both a jsoniter codec (snake_case) and a zio-json codec (camelCase): the encoded field name shows which
    * one Okapi picked.
    */
  final case class Book(pageCount: Int)

  object Book {
    given JsonValueCodec[Book] =
      JsonCodecMaker.make(CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case))
    given zio.json.JsonCodec[Book] = zio.json.DeriveJsonCodec.gen
  }

  given JsonValueCodec[(Int, String)] = JsonCodecMaker.make

  final case class Author(name: String, born: Int) derives JsoniterCodec

  sealed trait Shape derives JsoniterCodec
  final case class Circle(radius: Double) extends Shape
  final case class Square(side: Double) extends Shape
  case object Dot extends Shape

  enum Color derives JsoniterCodec {
    case Red, Green
  }

  final case class Drawing(shapes: List[Shape], color: Color) derives JsoniterCodec

  final case class Page(offset: Int = 0, size: Int) derives JsoniterCodec

  object Consts {
    final val Base = "/const"
    final val Accepted = 202
  }

  @Controller("/core")
  final class CoreApi[F[_]](using F: OkapiEffect[F]) {

    @Get("/book")
    def book: F[Book] = F.pure(Book(7))

    @Get("/author")
    def author: Author = Author("Herbert", 1920)

    @Get("/authors")
    def authors: List[Author] = List(Author("Herbert", 1920))

    @Get("/authors-by-name")
    def authorsByName: Map[String, Option[Author]] = Map("h" -> Some(Author("Herbert", 1920)))

    @Post("/shape")
    def shape(@RequestBody shape: Shape): Shape = shape

    @Get("/shapes")
    def shapes: List[Shape] = List(Circle(1), Dot)

    @Get("/drawing")
    def drawing: Drawing = Drawing(List(Square(2)), Color.Red)

    @Get("/paged")
    def paged: Page = Page(0, 10)

    @Get("/count")
    def count: F[Int] = F.pure(3)

    @Get("/tag-names")
    def tagNames: List[String] = List("a", "b")

    @Post("/sum")
    def sum(@RequestBody numbers: List[Int]): Int = numbers.sum

    @Get("/problem")
    @Produces("application/problem+json")
    def problem: Author = Author("Herbert", 1920)

    @Post("/charset")
    @Consumes("application/json; charset=utf-8")
    def charset(@RequestBody author: Author): String = author.name

    @Post("/created")
    def created: ApiResponse[String] = ApiResponse("made")

    @Delete("/gone")
    def gone: ApiResponse[Unit] = ApiResponse(())

    @Get("/accepted")
    @Status(Consts.Accepted)
    def accepted: ApiResponse[String] = ApiResponse("later")

    @Get("/download")
    def download(@Query("name") name: String): FileResponse = FileResponse(Array[Byte](1), name)

    @Get("/raw-json")
    @Produces("application/json")
    def rawJson: String = """{"already":"json"}"""

    @Get("/png")
    @Produces("image/png")
    def png: Array[Byte] = Array[Byte](1, 2)

    @Get(Consts.Base)
    @Status(Consts.Accepted)
    def constant: String = "const"

    @Get("/page")
    def page(@Query("offset") offset: Int, @Query("limit") limit: Int = 20): String = s"$offset+$limit"

    @Get("/filter")
    def filter(
      @Query("from") from: Option[Int] = None,
      @Query("size") size: Option[Int] = Some(10),
      @Query("tag") tags: List[String] = Nil,
      @Header("X-Sort") sort: List[String] = List("id"),
    ): String = s"$from/$size/${tags.mkString(",")}/${sort.mkString(",")}"

    @Post("/pair/{id}")
    def pair(@Path("id") id: Int, @RequestBody pair: (Int, String)): String = s"$id:${pair._1}:${pair._2}"

    @Get("/extra")
    def extra(@Path("rest") rest: String): String = s"extra:$rest"

    @Get("/extra/stats")
    def stats: String = "stats"
  }

  /** An annotated API trait implemented by an unannotated class. */
  trait LibraryRoutes[F[_]] {
    @Get("/shelf/{n}")
    def shelf(@Path("n") n: Int): F[String]
  }

  @Controller("/library")
  final class Library extends LibraryRoutes[Try] {
    def shelf(n: Int): Try[String] = Success(s"shelf $n")
  }

  private given OkapiEffect[Try] = OkapiEffect.fromMonadError[Try](using TryMonad)

  private val core: List[ServerEndpoint[Any, Try]] = OkapiEndpoints[Try].of(CoreApi[Try]())

  /** Another JSON integration imported at the expansion site takes precedence over the jsoniter default. */
  private object SwappedJson {
    import sttp.tapir.json.zio.*
    val endpoints: List[ServerEndpoint[Any, Try]] = OkapiEndpoints[Try].of(CoreApi[Try]())
  }

  private def find(endpoints: List[ServerEndpoint[Any, Try]], path: String): ServerEndpoint[Any, Try] =
    endpoints.find(_.endpoint.showPathTemplate(showQueryParam = None) == path).get

  private def run(se: ServerEndpoint[Any, Try], input: Any): Try[Either[Any, Any]] =
    se.logic(TryMonad)(().asInstanceOf[se.PRINCIPAL])(input.asInstanceOf[se.INPUT])

  private def body(transput: EndpointTransput[?]): EndpointIO.Body[?, ?] =
    atoms(transput).collect { case b: EndpointIO.Body[?, ?] => b }.head

  private def atoms(transput: EndpointTransput[?]): List[EndpointTransput[?]] = {
    transput match {
      case EndpointOutput.Pair(left, right, _, _) => atoms(left) ++ atoms(right)
      case EndpointInput.Pair(left, right, _, _) => atoms(left) ++ atoms(right)
      case EndpointIO.Pair(left, right, _, _) => atoms(left) ++ atoms(right)
      case EndpointOutput.MappedPair(pair, _) => atoms(pair)
      case EndpointInput.MappedPair(pair, _) => atoms(pair)
      case EndpointIO.MappedPair(pair, _) => atoms(pair)
      case other => List(other)
    }
  }

  /** The status codes documented on the endpoint's per-call status output. */
  private def documentedStatuses(se: ServerEndpoint[Any, Try]): Set[StatusCode] = {
    atoms(se.endpoint.output)
      .collect { case s: EndpointOutput.StatusCode[?] => s.documentedCodes.keySet }
      .flatten
      .collect { case Left(code) => code }
      .toSet
  }

  private def encode(se: ServerEndpoint[Any, Try], value: Any): Any =
    body(se.endpoint.output).codec.asInstanceOf[sttp.tapir.Codec[Any, Any, ?]].encode(value)

  private def discriminator(schema: sttp.tapir.Schema[?]): Option[(String, Set[String])] = {
    schema.schemaType match {
      case c: sttp.tapir.SchemaType.SCoproduct[?] => c.discriminator.map(d => d.name.name -> d.mapping.keySet)
      case _ => None
    }
  }

  private def mediaType(se: ServerEndpoint[Any, Try]): String = body(se.endpoint.output).codec.format.mediaType.toString

  override def spec: Spec[Any, Any] = {
    suite("CoreFeaturesSpec")(
      suite("JSON")(
        test("jsoniter-scala is the default JSON codec") {
          assertTrue(encode(find(core, "/core/book"), Book(7)) == """{"page_count":7}""")
        },
        test("derives JsoniterCodec") {
          assertTrue(
            encode(find(core, "/core/author"), Author("Herbert", 1920)) == """{"name":"Herbert","born":1920}"""
          )
        },
        test("containers of JsoniterCodec types get a codec made on the spot") {
          val list = encode(find(core, "/core/authors"), List(Author("H", 1)))
          val map = encode(find(core, "/core/authors-by-name"), Map("h" -> Some(Author("H", 1))))
          assertTrue(list == """[{"name":"H","born":1}]""", map == """{"h":{"name":"H","born":1}}""")
        },
        test("sealed hierarchies and enums carry a discriminator, in the codec and in the schema alike") {
          val drawing = find(core, "/core/drawing")
          val json = encode(drawing, Drawing(List(Circle(1), Dot), Color.Green))
          val shapeSchema = body(find(core, "/core/shape").endpoint.input).codec.schema
          assertTrue(
            json == """{"shapes":[{"type":"Circle","radius":1.0},{"type":"Dot"}],"color":{"type":"Green"}}""",
            discriminator(shapeSchema) == Some("type" -> Set("Circle", "Square", "Dot")),
          )
        },
        test("a sealed body decodes to the right case") {
          val codec =
            body(find(core, "/core/shape").endpoint.input).codec.asInstanceOf[sttp.tapir.Codec[String, Shape, ?]]
          assertTrue(codec.decode("""{"type":"Square","side":3.0}""") == sttp.tapir.DecodeResult.Value(Square(3)))
        },
        test("a list of a sealed type") {
          assertTrue(
            encode(
              find(core, "/core/shapes"),
              List(Circle(1), Dot),
            ) == """[{"type":"Circle","radius":1.0},{"type":"Dot"}]"""
          )
        },
        test("a field equal to its default value is written, as the schema requires it") {
          assertTrue(encode(find(core, "/core/paged"), Page(0, 10)) == """{"offset":0,"size":10}""")
        },
        test("primitives and containers of primitives get a codec made on the spot") {
          val sum = body(find(core, "/core/sum").endpoint.input).codec.asInstanceOf[sttp.tapir.Codec[String, Any, ?]]
          assertTrue(
            encode(find(core, "/core/count"), 3) == "3",
            encode(find(core, "/core/tag-names"), List("a", "b")) == """["a","b"]""",
            sum.decode("[1,2]") == DecodeResult.Value(List(1, 2)),
          )
        },
        test("an imported Tapir JSON integration replaces it") {
          assertTrue(encode(find(SwappedJson.endpoints, "/core/book"), Book(7)) == """{"pageCount":7}""")
        },
        test("the error body does not depend on the application's JSON library") {
          val error = find(SwappedJson.endpoints, "/core/book").endpoint.errorOutput
          val codec = body(error).codec.asInstanceOf[sttp.tapir.Codec[String, Any, ?]]
          assertTrue(codec.encode(http.ApiError.ApiErrorResponse(404, "no")) == """{"code":404,"message":"no"}""")
        },
      ),
      suite("media types")(
        test("an explicit application/json on a String keeps it as is, with a JSON content type") {
          val se = find(core, "/core/raw-json")
          assertTrue(mediaType(se) == "application/json", encode(se, """{"a":1}""") == """{"a":1}""")
        },
        test("a typed body with a +json media type is served with that media type") {
          val se = find(core, "/core/problem")
          assertTrue(
            mediaType(se) == "application/problem+json",
            encode(se, Author("H", 1)) == """{"name":"H","born":1}""",
          )
        },
        test("a JSON media type with parameters is JSON") {
          val codec = body(find(core, "/core/charset").endpoint.input).codec
          assertTrue(
            codec.format.mediaType.toString == "application/json; charset=utf-8",
            codec.asInstanceOf[sttp.tapir.Codec[String, Any, ?]].decode("""{"name":"H","born":1}""") ==
              DecodeResult.Value(Author("H", 1)),
          )
        },
        test("a binary body is served with its declared media type") {
          assertTrue(mediaType(find(core, "/core/png")) == "image/png")
        },
        test("a non-String result with a text media type is a compile error") {
          typeCheck {
            """
            @Controller("/c") final class C { @Get("/x") @Produces("text/plain") def x: Book = Book(1) }
            OkapiEndpoints[Try].of(C())
            """
          }.map(result => assertTrue(result.left.exists(_.contains("""@Produces("text/plain") cannot encode"""))))
        },
      ),
      suite("annotations")(
        test("constant annotation arguments (final vals)") {
          val output = find(core, "/core/const").endpoint.output.show
          assertTrue(output.contains("202"))
        },
        test("an ApiResponse documents the endpoint's success status") {
          assertTrue(
            documentedStatuses(find(core, "/core/created")) == Set(StatusCode.Created),
            documentedStatuses(find(core, "/core/gone")) == Set(StatusCode.NoContent),
            documentedStatuses(find(core, "/core/accepted")) == Set(StatusCode.Accepted),
          )
        },
        test("annotations are inherited from the API trait a controller implements") {
          val library = OkapiEndpoints[Try].of(Library())
          assertTrue(
            library.map(_.endpoint.showPathTemplate(showQueryParam = None)) == List("/library/shelf/{n}"),
            run(library.head, 3) == Success(Right("shelf 3")),
          )
        },
      ),
      suite("parameters")(
        test("a parameter with a default value is optional and falls back to the default") {
          val se = find(core, "/core/page")
          assertTrue(
            run(se, (10, None)) == Success(Right("10+20")),
            run(se, (10, Some(5))) == Success(Right("10+5")),
          )
        },
        test("an Option or collection parameter with a default decodes as itself and falls back when empty") {
          val se = find(core, "/core/filter")
          assertTrue(
            run(se, (None, None, Nil, Nil)) == Success(Right("None/Some(10)//id")),
            run(se, (Some(1), Some(2), List("a", "b"), List("x"))) == Success(Right("Some(1)/Some(2)/a,b/x")),
          )
        },
        test("a tuple-typed body next to another input arrives flattened and is rebuilt") {
          assertTrue(run(find(core, "/core/pair/{id}"), (1, 2, "x")) == Success(Right("1:2:x")))
        },
      ),
      suite("downloads")(
        test("the filename keeps printable ASCII without quotes and backslashes, and non-ASCII goes in filename*") {
          val se = find(core, "/core/download")
          def disposition(name: String): Any = run(se, name).map(_.map(_.asInstanceOf[(Array[Byte], String)]._2))
          assertTrue(
            disposition("report\\") == Success(Right("attachment; filename=\"report_\"")),
            disposition(s"a${0.toChar}b.txt") == Success(Right("attachment; filename=\"a_b.txt\"")),
            disposition("zażółć.pdf") ==
              Success(Right("attachment; filename=\"za____.pdf\"; filename*=UTF-8''za%C5%BC%C3%B3%C5%82%C4%87.pdf")),
          )
        }
      ),
      suite("routing")(
        test("routes are ordered by their effective path, including appended @Path captures") {
          val paths = core.map(_.endpoint.showPathTemplate(showQueryParam = None))
          assertTrue(paths.indexOf("/core/extra/stats") < paths.indexOf("/core/extra/{rest}"))
        }
      ),
    )
  }
}
