package io.okapi.exampleclient

import io.okapi.exampleclient.api.ExampleApi.*
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri
import sttp.tapir.client.sttp4.SttpClientInterpreter

/** Talks to a running okapi-example (`sbt run` in okapi-example/) through the generated `ExampleApi` endpoints.
  * The base URL is the first argument, `http://localhost:38081` by default.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val baseUri = Some(Uri.unsafeParse(args.headOption.getOrElse("http://localhost:38081")))
    val backend = DefaultSyncBackend()
    val client = SttpClientInterpreter()

    val created = client.toClientThrowDecodeFailures(postApiBooks, baseUri, backend)(
      CreateBookRequest(title = "Dune", author = "Frank Herbert", genre = "scifi", year = 1965)
    )
    println(s"created:  $created")
    println(s"books:    ${client.toClientThrowDecodeFailures(getApiBooks, baseUri, backend)((None, None))}")
    created.foreach(book => println(s"by id:    ${client.toClientThrowDecodeFailures(getApiBooksId, baseUri, backend)(book.id)}"))
    println(s"missing:  ${client.toClientThrowDecodeFailures(getApiBooksId, baseUri, backend)(999)}")
    println(s"stats:    ${client.toClientThrowDecodeFailures(getApiBooksStats, baseUri, backend)(())}")
  }
}
