package io.okapi.sbt

import java.io.File

import sbt.io.IO
import sttp.tapir.codegen.{ RootGenerator, YamlParser }
import sttp.tapir.codegen.dedup.PackageReuseContext

/** Writes a client module — Tapir endpoint definitions, models with jsoniter-scala codecs, and a `build.sbt` — from an
  * OpenAPI document.
  */
object OkapiClientGenerator {

  /** @return
    *   the files written under `directory`: one source per generated object, plus `build.sbt` when it did not exist.
    */
  def generate(
    spec: String,
    packageName: String,
    objectName: String,
    moduleName: String,
    scalaVersion: String,
    streaming: String,
    directory: File,
  ): Seq[File] = {
    if (!Streaming.contains(streaming))
      sys.error(s"okapiClientStreaming must be one of ${Streaming.keys.mkString(", ")}")
    val document = YamlParser.parseFile(spec) match {
      case Right(document) => document
      case Left(error) => sys.error(s"The OpenAPI document cannot be parsed: ${error.getMessage}")
    }
    val generated = RootGenerator.generateObjects(
      unNormalisedDoc = document,
      packagePath = packageName,
      objName = objectName,
      targetScala3 = true,
      useHeadTagForObjectNames = false,
      jsonSerdeLib = "jsoniter",
      xmlSerdeLib = "none",
      streamingImplementation = streaming,
      validateNonDiscriminatedOneOfs = true,
      maxSchemasPerFile = 400,
      generateEndpointTypes = false,
      generateValidators = true,
      useCustomJsoniterSerdes = false,
      packageReuse = PackageReuseContext.none,
      seperateFilesForModels = false,
      alwaysGenerateParamSupport = false,
      addDisambiguationCodes = false,
    )
    val sourceRoot = new File(directory, "src/main/scala/" + packageName.replace('.', '/'))
    val sources = generated.allFiles.toSeq.map {
      case (name, body) =>
        val file = new File(sourceRoot, name.split('.').mkString("/") + ".scala")
        IO.write(file, body)
        file
    }
    val build = new File(directory, "build.sbt")
    val buildFile =
      if (build.exists) Nil else { IO.write(build, buildSbt(moduleName, scalaVersion, streaming)); Seq(build) }
    sources ++ buildFile
  }

  /** Streams of binary bodies: the `sttp.capabilities` implementation the generated endpoints use. */
  val Streaming: Map[String, String] = Map(
    "fs2" -> s""""com.softwaremill.sttp.shared" %% "fs2" % "${OkapiVersions.sttpShared}"""",
    "zio" -> s""""com.softwaremill.sttp.shared" %% "zio" % "${OkapiVersions.sttpShared}"""",
  )

  private def buildSbt(moduleName: String, scalaVersion: String, streaming: String): String = {
    s"""name := "$moduleName"
       |scalaVersion := "$scalaVersion"
       |
       |libraryDependencies ++= Seq(
       |  "com.softwaremill.sttp.tapir" %% "tapir-core" % "${OkapiVersions.tapir}",
       |  "com.softwaremill.sttp.tapir" %% "tapir-sttp-client4" % "${OkapiVersions.tapir}",
       |  "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "${OkapiVersions.tapir}",
       |  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "${OkapiVersions.jsoniter}",
       |  ${Streaming(streaming)},
       |)
       |""".stripMargin
  }
}
