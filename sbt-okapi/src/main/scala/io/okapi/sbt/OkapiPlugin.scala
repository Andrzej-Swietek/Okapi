package io.okapi.sbt

import sbt._
import sbt.Keys._

/** sbt tasks for an Okapi API:
  *   - `okapiSpecFile` writes its OpenAPI document to `target/okapi/openapi.yaml`;
  *   - `okapiGenerateClient` generates the `<name>-client` module from it, as files under
  *     [[autoImport.okapiClientDirectory]].
  *
  * Enable with `enablePlugins(OkapiPlugin)` and set `okapiSpec` to a `String` expression of the project, e.g.
  * `okapiSpec := "com.example.ApiSpec.yaml"` with
  * `object ApiSpec { def yaml = Okapi.openApiYaml[Controllers]("API", "1") }`.
  */
object OkapiPlugin extends AutoPlugin {

  object autoImport {
    val okapiSpec = settingKey[String]("A String expression of this project evaluating to the API's OpenAPI document")
    val okapiSpecFile = taskKey[File]("Writes the API's OpenAPI document to target/okapi/openapi.yaml")
    val okapiClientName = settingKey[String]("Name of the generated client module")
    val okapiClientPackage = settingKey[String]("Package of the generated client")
    val okapiClientObject = settingKey[String]("Object holding the generated endpoints")
    val okapiClientDirectory = settingKey[File]("Directory the generated client module is written to")
    val okapiClientStreaming = settingKey[String]("Streams for binary bodies in the client: fs2 (default) or zio")
    val okapiGenerateClient = taskKey[Seq[File]]("Generates the client module from the API's OpenAPI document")
  }

  import autoImport._

  private val Dumper = "io.okapi.sbt.generated.OkapiSpecDump"

  override def projectSettings: Seq[Setting[_]] = Seq(
    okapiClientName := s"${name.value}-client",
    okapiClientPackage := identifier(name.value).toLowerCase + ".client",
    okapiClientObject := identifier(name.value).capitalize + "Endpoints",
    okapiClientDirectory := baseDirectory.value / okapiClientName.value,
    okapiClientStreaming := "fs2",
    // in Test, so the production artifact does not carry it
    Test / sourceGenerators += Def.task {
      okapiSpec.?.value.toSeq.map { expression =>
        val file = (Test / sourceManaged).value / "okapi" / "OkapiSpecDump.scala"
        IO.write(file, dumper(expression))
        file
      }
    }.taskValue,
    okapiSpecFile := Def.taskDyn {
      val out = target.value / "okapi" / "openapi.yaml"
      if (okapiSpec.?.value.isEmpty) sys.error("Set okapiSpec to the expression giving the API's OpenAPI document")
      IO.createDirectory(out.getParentFile)
      Def.task {
        (Test / runMain).toTask(s" $Dumper ${out.getAbsolutePath}").value
        out
      }
    }.value,
    okapiGenerateClient := {
      val generated = OkapiClientGenerator.generate(
        spec = IO.read(okapiSpecFile.value),
        packageName = okapiClientPackage.value,
        objectName = okapiClientObject.value,
        moduleName = okapiClientName.value,
        scalaVersion = scalaVersion.value,
        streaming = okapiClientStreaming.value,
        directory = okapiClientDirectory.value,
      )
      streams.value.log.info(s"Okapi: wrote ${generated.size} files to ${okapiClientDirectory.value}")
      generated
    },
  )

  private def dumper(expression: String): String = {
    s"""package io.okapi.sbt.generated
       |
       |object OkapiSpecDump {
       |  def main(args: Array[String]): Unit = {
       |    val _ = java.nio.file.Files.writeString(java.nio.file.Paths.get(args(0)), $expression)
       |  }
       |}
       |""".stripMargin
  }

  private def identifier(name: String): String = {
    name
      .split("[^A-Za-z0-9]+")
      .filter(_.nonEmpty)
      .zipWithIndex
      .map { case (part, i) => if (i == 0) part else part.capitalize }
      .mkString
  }
}
