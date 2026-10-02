package io.okapi.codegen

import java.io.File

/** Writes the clients of [[OkapiCodegenSpec.document]] under the directory `args(0)`, one per streaming mode:
  * `books.client` (none), `books.fs2client` and `books.zioclient`.
  */
object GenerateFixture {
  def main(args: Array[String]): Unit = {
    val directory = new File(args(0))
    List("books.client" -> Streaming.Disabled, "books.fs2client" -> Streaming.Fs2, "books.zioclient" -> Streaming.Zio)
      .foreach { (pkg, streaming) =>
        val settings = OkapiCodegenSpec.settings(directory, streaming = streaming, pkg = pkg)
        val written = OkapiCodegen.generate(OkapiCodegenSpec.document, settings.copy(buildFile = BuildFile.Never))
        written.left.foreach(e => throw new IllegalStateException(e))
      }
  }
}
