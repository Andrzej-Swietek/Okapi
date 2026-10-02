package io.okapi.codegen

import zio.test.*

// the specs drive JDK and sttp APIs typed with nulls
import scala.language.unsafeNulls
import books.client.*
import books.client.impl.SttpLibrary
import books.client.models.*
import sttp.client4.*
import sttp.client4.testing.SyncBackendStub
import sttp.model.{ Header, Method, StatusCode }
import scala.collection.mutable

object GeneratedClientSpec extends ZIOSpecDefault {

  private val bookJson =
    """{"id":1,"title":"Dune","page_count":412,"shape":{"type":"Circle","radius":2.0},"sequel":{"id":2,"title":"Messiah","shape":{"type":"Dot"}}}"""

  /** A client over a stub answering `response` and recording each request as `METHOD uri headers :: body`. */
  private def client(response: String, status: StatusCode = StatusCode.Ok)
    : (Library[sttp.shared.Identity], mutable.Buffer[String]) = {
    val sent = mutable.Buffer.empty[String]
    val backend = SyncBackendStub
      .whenRequestMatches { r =>
        val body = r.body match {
          case b: ByteArrayBody => new String(b.b)
          case m: MultipartBody[?] => m.parts.map(p => p.name + p.fileName.fold("")(f => s"[$f]")).mkString(",")
          case NoBody => ""
          case other => other.toString
        }
        val headers = r.headers.filter(h => h.name.startsWith("X-") || h.name == "Authorization").mkString(" ")
        sent += s"${r.method} ${r.uri} $headers :: $body"
        true
      }
      .thenRespondAdjust(response, status)
    (SttpLibrary(backend, uri"http://api/base", Seq(Header("Authorization", "Bearer t"))), sent)
  }

  def spec = {
    suite("GeneratedClientSpec")(
      test("decodes sealed, recursive and renamed fields") {
        val (api, _) = client(s"[$bookJson]")
        val books = api.books.listBooks("r")
        assertTrue(
          books == List(Book(1, "Dune", Some(412), Nil, Circle(2), Some(Book(2, "Messiah", None, Nil, Dot, None))))
        )
      },
      test("sends the path, query, headers and JSON body, with the discriminator") {
        val (lists, listed) = client("[]")
        val (creates, created) = client(bookJson)
        val _ = lists.books.listBooks("r-1", Some(Genre.SciFi))
        val _ = lists.books.listBooks("r-2")
        val _ = creates.books.createBook(Book(3, "Children", Some(5), Nil, Circle(1)))
        val sent = listed ++ created
        assertTrue(
          sent.toList == List(
            "GET http://api/base/api/books?genre=sci-fi Authorization: Bearer t X-Request-Id: r-1 :: ",
            "GET http://api/base/api/books Authorization: Bearer t X-Request-Id: r-2 :: ",
            """POST http://api/base/api/books Authorization: Bearer t :: {"id":3,"title":"Children","page_count":5,"shape":{"type":"Circle","radius":1.0}}""",
          )
        )
      },
      test("sends a multipart form, with the binary field as a file part") {
        val (api, sent) = client("ok")
        val reply = api.cover.uploadCover(7, Array[Byte](1, 2), Some("front"))
        assertTrue(
          reply == "ok",
          sent.toList == List("POST http://api/base/api/books/7/cover Authorization: Bearer t :: file[file],caption"),
        )
      },
      test("an empty response is Unit; a binary one is its bytes") {
        val (empty, _) = client("", StatusCode.NoContent)
        val (binary, _) = client("png")
        assertTrue(empty.books.deleteBook(1) == (), binary.cover.cover(1).toList == "png".getBytes.toList)
      },
      test("with every operation on the root trait, the client sends the same requests") {
        val backend = SyncBackendStub
          .whenRequestMatches(r => r.method == Method.DELETE && r.uri.toString == "http://api/base/api/books/3")
          .thenRespondAdjust("", StatusCode.NoContent)
        val api: _root_.books.single.Library[sttp.shared.Identity] =
          _root_.books.single.impl.SttpLibrary(backend, uri"http://api/base")
        assertTrue(api.booksDeleteBook(3) == ())
      },
      test("a non-2xx response fails with ApiException, its ApiErrorResponse decoded") {
        val (api, _) = client("""{"code":404,"message":"Book 9 not found"}""", StatusCode.NotFound)
        val error = scala.util.Try(api.books.deleteBook(9)).failed.get.asInstanceOf[ApiException]
        assertTrue(error.status == 404, error.error == Some(ApiErrorResponse(404, "Book 9 not found")))
      },
    )
  }
}
