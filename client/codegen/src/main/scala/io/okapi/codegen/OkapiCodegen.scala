package io.okapi.codegen

import java.io.File
import java.nio.file.Files

import io.okapi.codegen.render.*
import sttp.tapir.codegen.YamlParser

/** Generates a client module from an OpenAPI document: tagless-final API traits, models with jsoniter-scala codecs, an
  * sttp client4 implementation, and a `build.sbt` (see [[Settings.buildFile]]).
  */
object OkapiCodegen {

  /** @return
    *   the files written, or why nothing was.
    */
  def generate(spec: String, settings: Settings): Either[String, List[File]] = {
    for {
      document <- YamlParser
        .parseFile(spec)
        .left
        .map(e => s"The OpenAPI document cannot be parsed, so no client is written: ${e.getMessage}")
    } yield {
      val sources = render(ContractReader(document).read(settings.grouping, settings.api), settings)
      if (settings.clean) delete(new File(settings.sourceDirectory, settings.packages.root.value.replace('.', '/')))
      val written = sources.map { source =>
        val file = new File(settings.sourceDirectory, source.path)
        file.getParentFile.mkdirs()
        Files.writeString(file.toPath, source.content)
        file
      }
      written ++ writeBuild(settings)
    }
  }

  private def render(model: ClientModel, settings: Settings): List[SourceFile] = {
    val packages = settings.packages
    ApiRenderer.render(model, settings) ++
      ModelRenderer.render(model, packages, settings.separateModels) ++
      ImplRenderer.render(model, settings)
  }

  private def writeBuild(settings: Settings): List[File] = {
    val build = new File(settings.directory, "build.sbt")
    val write = settings.buildFile match {
      case BuildFile.Always => true
      case BuildFile.IfMissing => !build.exists
      case BuildFile.Never => false
    }
    if (write) {
      build.getParentFile.mkdirs()
      Files.writeString(build.toPath, BuildRenderer.render(settings.module, settings.streaming))
      List(build)
    }
    else Nil
  }

  private def delete(file: File): Unit = {
    Option(file.listFiles).foreach(_.foreach(delete))
    val _ = file.delete()
  }

  /** The entry point of a runner holding the OpenAPI document `spec`:
    *   - `spec <file>` writes the document to `file`;
    *   - `client <properties>` generates the client with the settings [[Settings.load]] reads from `properties`.
    *
    * Throws `IllegalArgumentException` on any other arguments, or when the settings or the document are invalid.
    */
  def run(args: Array[String], spec: => String): Unit = {
    args.toList match {
      case List("spec", out) =>
        val file = new File(out)
        file.getParentFile.mkdirs()
        val _ = Files.writeString(file.toPath, spec)
      case List("client", properties) =>
        Settings.load(new File(properties)).flatMap(generate(spec, _)) match {
          case Right(files) => println(s"Okapi: wrote ${files.size} files")
          case Left(error) => throw new IllegalArgumentException(error)
        }
      case other =>
        throw new IllegalArgumentException(
          s"Unknown arguments '${other.mkString(" ")}', nothing is written: pass `spec <file>` or `client <properties>`"
        )
    }
  }
}
