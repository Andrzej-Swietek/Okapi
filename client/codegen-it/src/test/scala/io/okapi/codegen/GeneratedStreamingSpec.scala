package io.okapi.codegen

import zio.ZIO
import zio.stream.ZStream
import zio.test.*

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8

import scala.collection.mutable
// the specs drive JDK and sttp APIs typed with nulls
import scala.language.unsafeNulls
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.sun.net.httpserver.{ HttpExchange, HttpServer }
import sttp.client4.httpclient.fs2.HttpClientFs2Backend
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri

object GeneratedStreamingSpec extends ZIOSpecDefault {

  private val uploads = mutable.Buffer.empty[String]

  private lazy val server: HttpServer = {
    val s = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
    def reply(e: HttpExchange, status: Int, contentType: String, body: String): Unit = {
      val bytes = body.getBytes(UTF_8)
      e.getResponseHeaders.add("Content-Type", contentType)
      e.sendResponseHeaders(status, if (status == 204) -1 else bytes.length.toLong)
      if (status != 204) e.getResponseBody.write(bytes)
      e.close()
    }
    s.createContext(
      "/",
      e =>
        (e.getRequestMethod, e.getRequestURI.getPath) match {
          case ("GET", "/api/books/7/content") => reply(e, 200, "application/octet-stream", "chunk-1chunk-2")
          case ("PUT", "/api/books/7/content") =>
            uploads.synchronized { uploads += new String(e.getRequestBody.readAllBytes(), UTF_8); () }
            reply(e, 204, "text/plain", "")
          case ("GET", "/api/books/events") =>
            reply(e, 200, "text/event-stream", "data: first\n\nevent: update\ndata: second\n\n")
          case _ => reply(e, 404, "application/json", """{"code":404,"message":"no such thing"}""")
        },
    )
    s.start()
    s
  }

  private def base: Uri = Uri.unsafeParse(s"http://localhost:${server.getAddress.getPort}")

  def spec = {
    suite("GeneratedStreamingSpec")(
      test("fs2: streams a download and an upload, and reads server-sent events") {
        val run = HttpClientFs2Backend.resource[IO]().use { backend =>
          val api = books.fs2client.impl.SttpLibrary[IO](backend, base)
          for {
            download <- api.books.downloadContent(7).flatMap(_.compile.to(Array))
            _ <- api.books.uploadContent(7, fs2.Stream.emits("up-fs2".getBytes(UTF_8)).covary[IO])
            events <- api.books.bookEvents().flatMap(_.compile.toList)
            missing <- api.books.downloadContent(9).attempt
          } yield (new String(download, UTF_8), events, missing)
        }
        val (download, events, missing) = run.unsafeRunSync()
        assertTrue(
          download == "chunk-1chunk-2",
          uploads.contains("up-fs2"),
          events.map(e => (e.eventType, e.data)) == List(None -> Some("first"), Some("update") -> Some("second")),
          missing.left.toOption.collect { case e: books.fs2client.ApiException => e.status }.contains(404),
        )
      },
      test("zio: streams a download and an upload, and reads server-sent events") {
        for {
          backend <- HttpClientZioBackend.scoped()
          api = books.zioclient.impl.SttpLibrary(backend, base)
          download <- api.books.downloadContent(7).flatMap(_.runCollect)
          _ <- api.books.uploadContent(7, ZStream.fromIterable("up-zio".getBytes(UTF_8)))
          events <- api.books.bookEvents().flatMap(_.runCollect)
          missing <- api.books.downloadContent(9).flip
        } yield assertTrue(
          new String(download.toArray, UTF_8) == "chunk-1chunk-2",
          uploads.contains("up-zio"),
          events.map(_.data).toList == List(Some("first"), Some("second")),
          missing.asInstanceOf[books.zioclient.ApiException].error.map(_.message).contains("no such thing"),
        )
      },
      test("none: a streamed body is a byte array, the events are read whole") {
        val backend = sttp.client4.DefaultSyncBackend()
        val api = books.client.impl.SttpLibrary(backend, base)
        val (download, events) =
          try (api.books.downloadContent(7), api.books.bookEvents())
          finally backend.close()
        assertTrue(
          new String(download, UTF_8) == "chunk-1chunk-2",
          events.map(_.data) == List(Some("first"), Some("second")),
        )
      },
    ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.afterAll(ZIO.succeed(server.stop(0)))
  }
}
