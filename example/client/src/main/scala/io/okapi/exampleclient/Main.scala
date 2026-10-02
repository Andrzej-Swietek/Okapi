package io.okapi.exampleclient

import zio.{ Console, Task, ZIO, ZIOAppDefault }
import zio.stream.ZStream

import io.okapi.exampleclient.api.{ ApiException, ExampleApi }
import io.okapi.exampleclient.api.impl.SttpExampleApi
import io.okapi.exampleclient.api.models.CreateBookRequest
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.model.Uri

/** Calls the running example API (`sbt run` in example/server/) through the generated client. The base URL is the
  * first argument, `http://localhost:38081` by default.
  */
object Main extends ZIOAppDefault {

  def run = {
    for {
      args <- getArgs
      backend <- HttpClientZioBackend()
      api: ExampleApi[Task] = SttpExampleApi(
        backend,
        Uri.unsafeParse(args.headOption.getOrElse("http://localhost:38081")),
      )
      created <- api.books.createBook(CreateBookRequest("Dune", "Frank Herbert", "scifi", 1965))
      _ <- Console.printLine(s"created:  $created")
      _ <- api.books.listBooks().flatMap(books => Console.printLine(s"books:    $books"))
      _ <- api.books.getBook(created.id).flatMap(book => Console.printLine(s"by id:    $book"))
      _ <- api.books.getBook(999).catchSome {
        case e: ApiException =>
          Console.printLine(s"missing:  ${e.status} ${e.error.map(_.message).getOrElse(e.body)}").as(created)
      }
      cover = ZStream.fromIterable("a streamed cover".getBytes)
      uploaded <- api.covers.uploadCoverStreaming(created.id, cover, Some("Dune"))
      _ <- Console.printLine(s"streamed: $uploaded")
      _ <- api.books.stats().flatMap(stats => Console.printLine(s"stats:    $stats"))
    } yield ()
  }
}
