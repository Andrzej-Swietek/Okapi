package io.okapi.codegen

import java.io.File

import scala.io.Source
import scala.util.Using

/** Writes clients under the directory in `args(0)`: [[OkapiCodegenSpec.document]] in every streaming mode, in
  * `books.client` (none), `books.fs2client` and `books.zioclient`, and with every operation on the root trait and one
  * models file in `books.single` (none) and `books.fs2single`; and `hostile.yaml` (names that collide with Scala, sttp
  * and the generated code, and schemas that are hard to model) in `hostile.client`, `hostile.fs2client` and
  * `hostile.zioclient`.
  */
object GenerateFixture {

  private val Modes = List("client" -> Streaming.Disabled, "fs2client" -> Streaming.Fs2, "zioclient" -> Streaming.Zio)

  def main(args: Array[String]): Unit = {
    val directory = new File(args(0))
    val hostile = Using.resource(Source.fromResource("hostile.yaml"))(_.mkString)
    val split = for {
      (document, root, api) <- List((OkapiCodegenSpec.document, "books", "Library"), (hostile, "hostile", "Hostile"))
      (suffix, streaming) <- Modes
    } yield (document, OkapiCodegenSpec.settings(directory, streaming = streaming, pkg = s"$root.$suffix"), api)
    val single = List("single" -> Streaming.Disabled, "fs2single" -> Streaming.Fs2).map { (suffix, streaming) =>
      val settings = OkapiCodegenSpec.settings(directory, false, false, streaming, s"books.$suffix")
      (OkapiCodegenSpec.document, settings, "Library")
    }
    (split ++ single).foreach { (document, settings, api) =>
      val written =
        OkapiCodegen.generate(document, settings.copy(api = Identifier.tpe(api), buildFile = BuildFile.Never))
      written.left.foreach(e => throw new IllegalStateException(e))
    }
  }
}
