package io.okapi.sbt

import java.util.Properties

import sbt._
import sbt.Keys._

/** sbt tasks for an Okapi API:
  *   - `okapiSpecFile` writes its OpenAPI document to `target/okapi/openapi.yaml`;
  *   - `okapiGenerateClient` generates the client module from it under `okapiClientDirectory`, replacing the directory
  *     of `okapiClientPackage` unless `okapiClientClean` is off.
  *
  * Both run okapi-codegen, added to the project's Test dependencies, on the Test classpath. Enable with
  * `enablePlugins(OkapiPlugin)` and set `okapiSpec` to a `String` expression of the project, e.g.
  * `okapiSpec := "com.example.ApiSpec.yaml"` with
  * `object ApiSpec { def yaml = Okapi.openApiYaml[Controllers]("API", "1") }`.
  */
object OkapiPlugin extends AutoPlugin {

  object autoImport {
    val okapiSpec = settingKey[String]("A String expression of this project evaluating to the API's OpenAPI document")
    val okapiSpecFile = taskKey[File]("Writes the API's OpenAPI document to target/okapi/openapi.yaml")
    val okapiGenerateClient = taskKey[Seq[File]]("Generates the client module from the API's OpenAPI document")

    val okapiClientDirectory =
      settingKey[File]("Directory of the client module, holding its build.sbt (default: <base>/<okapiClientName>)")
    val okapiClientSourceDirectory =
      settingKey[File]("Source root of the client (default: <okapiClientDirectory>/src/main/scala)")
    val okapiClientName = settingKey[String]("Name of the client module (default: <name>-client)")
    val okapiClientOrganization =
      settingKey[String]("Organization of the client module (default: the project's); empty leaves it out")
    val okapiClientVersion =
      settingKey[String]("Version of the client module (default: the project's); empty leaves it out")
    val okapiClientScalaVersion = settingKey[String]("Scala version of the client module (default: the project's)")
    val okapiClientBuildFile =
      settingKey[String]("When build.sbt is written: if-missing (default), always, or never")

    val okapiClientPackage =
      settingKey[String]("Package of the client's API traits (default: <name>.client, lower case)")
    val okapiClientModelsPackage = settingKey[String]("Sub-package of the models (default: models)")
    val okapiClientImplPackage = settingKey[String]("Sub-package of the sttp implementation (default: impl)")
    val okapiClientApi =
      settingKey[String]("Root trait of the client, implemented by Sttp<okapiClientApi> (default: <Name>Client)")
    val okapiClientTitle = settingKey[String]("Name of the API in the client's scaladoc (default: okapiClientApi)")

    val okapiClientSplitByController =
      settingKey[Boolean]("A trait per controller, reached from the root trait (default), else one trait")
    val okapiClientControllerSuffix =
      settingKey[String]("Suffix of the controller traits: Covers -> CoversRoutes (default: Routes)")
    val okapiClientStreaming =
      settingKey[String](
        "Streamed bodies and server-sent events: fs2 (default), zio, or none (byte arrays, events read whole)"
      )
    val okapiClientSeparateModels = settingKey[Boolean]("A file per model (default), else one Models.scala")
    val okapiClientScalafmtConfig =
      settingKey[Option[File]](
        "The .scalafmt.conf formatting the client (default: the build root's if it exists); None uses a built-in one"
      )
    val okapiClientClean =
      settingKey[Boolean]("Deletes the directory of okapiClientPackage before generating (default: true)")
  }

  import autoImport._

  private val Runner = "io.okapi.sbt.generated.OkapiCodegenRunner"

  override def projectSettings: Seq[Setting[_]] = Seq(
    libraryDependencies += OkapiVersions.organization %% "okapi-codegen" % OkapiVersions.okapi % Test,
    okapiClientName := s"${name.value}-client",
    okapiClientDirectory := baseDirectory.value / okapiClientName.value,
    okapiClientSourceDirectory := okapiClientDirectory.value / "src" / "main" / "scala",
    okapiClientOrganization := organization.value,
    okapiClientVersion := version.value,
    okapiClientScalaVersion := scalaVersion.value,
    okapiClientBuildFile := "if-missing",
    okapiClientPackage := identifier(name.value).toLowerCase + ".client",
    okapiClientModelsPackage := "models",
    okapiClientImplPackage := "impl",
    okapiClientApi := identifier(name.value).capitalize + "Client",
    okapiClientTitle := okapiClientApi.value,
    okapiClientSplitByController := true,
    okapiClientControllerSuffix := "Routes",
    okapiClientStreaming := "fs2",
    okapiClientSeparateModels := true,
    okapiClientScalafmtConfig := Some((ThisBuild / baseDirectory).value / ".scalafmt.conf").filter(_.exists),
    okapiClientClean := true,
    // in Test, so the production artifact does not carry it
    Test / sourceGenerators += Def.task {
      okapiSpec.?.value.toSeq.map { expression =>
        val file = (Test / sourceManaged).value / "okapi" / "OkapiCodegenRunner.scala"
        IO.write(file, runner(expression))
        file
      }
    }.taskValue,
    okapiSpecFile := Def.taskDyn {
      requireSpec(okapiSpec.?.value)
      val out = target.value / "okapi" / "openapi.yaml"
      Def.task {
        (Test / runMain).toTask(s" $Runner spec ${out.getAbsolutePath}").value
        out
      }
    }.value,
    okapiGenerateClient := Def.taskDyn {
      requireSpec(okapiSpec.?.value)
      val properties = target.value / "okapi" / "client.properties"
      writeProperties(properties, clientProperties.value)
      val sources = okapiClientSourceDirectory.value / okapiClientPackage.value.replace('.', '/')
      val build = okapiClientDirectory.value / "build.sbt"
      val scalafmtConfig = okapiClientScalafmtConfig.value
      val fallbackConfig = target.value / "okapi" / "scalafmt.conf"
      Def.task {
        (Test / runMain).toTask(s" $Runner client ${properties.getAbsolutePath}").value
        val generated = (sources ** "*.scala").get
        ClientFormatter.format(generated, scalafmtConfig, fallbackConfig)
        generated ++ Seq(build).filter(_.exists)
      }
    }.value,
  )

  private def clientProperties = Def.setting {
    Map(
      "directory" -> okapiClientDirectory.value.getAbsolutePath,
      "sourceDirectory" -> okapiClientSourceDirectory.value.getAbsolutePath,
      "name" -> okapiClientName.value,
      "organization" -> okapiClientOrganization.value,
      "version" -> okapiClientVersion.value,
      "scalaVersion" -> okapiClientScalaVersion.value,
      "buildFile" -> okapiClientBuildFile.value,
      "package" -> okapiClientPackage.value,
      "modelsPackage" -> okapiClientModelsPackage.value,
      "implPackage" -> okapiClientImplPackage.value,
      "api" -> okapiClientApi.value,
      "title" -> okapiClientTitle.value,
      "splitByController" -> okapiClientSplitByController.value.toString,
      "controllerSuffix" -> okapiClientControllerSuffix.value,
      "streaming" -> okapiClientStreaming.value,
      "separateModels" -> okapiClientSeparateModels.value.toString,
      "clean" -> okapiClientClean.value.toString,
    )
  }

  private def requireSpec(spec: Option[String]): Unit =
    if (spec.isEmpty)
      sys.error(
        "okapiSpec is not set, so there is no OpenAPI document to work from. Set it to a String expression of this " +
          "project giving the document, e.g. okapiSpec := \"com.example.ApiSpec.yaml\""
      )

  private def writeProperties(file: File, values: Map[String, String]): Unit = {
    val properties = new Properties()
    values.foreach { case (key, value) => properties.setProperty(s"okapi.client.$key", value) }
    IO.createDirectory(file.getParentFile)
    IO.write(properties, "okapi-codegen settings, written by sbt-okapi", file)
  }

  private def runner(expression: String): String = {
    s"""package io.okapi.sbt.generated
       |
       |object OkapiCodegenRunner {
       |  def main(args: Array[String]): Unit = io.okapi.codegen.OkapiCodegen.run(args, $expression)
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
