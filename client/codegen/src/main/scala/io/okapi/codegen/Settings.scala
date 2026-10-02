package io.okapi.codegen

import java.io.{ File, FileInputStream }
import java.util.Properties

import scala.util.Using

/** When the generator writes the client module's `build.sbt`. */
enum BuildFile {

  /** Only when the module has none. */
  case IfMissing

  /** On every run, replacing the module's. */
  case Always

  /** Not at all: for a module that is a project of an existing build. */
  case Never
}

/** How operations are grouped into traits. */
enum Grouping {

  /** A trait per controller (the operations' first tag), named after it with `suffix` and reached from the root trait:
    * `Covers` → `trait CoversRoutes[F]` and `def covers: CoversRoutes[F]`.
    */
  case ByController(suffix: String)

  /** Every operation on the root trait. */
  case Single
}

object Grouping {
  val DefaultSuffix = "Routes"
}

/** How the client reads and sends streamed bodies (operations marked `force-*-streaming`) and server-sent events. */
enum Streaming {

  /** `fs2.Stream[F, Byte]` and `fs2.Stream[F, ServerSentEvent]`, over a `StreamBackend[F, Fs2Streams[F]]`. */
  case Fs2

  /** `ZStream[Any, Throwable, Byte]` and `ZStream[Any, Throwable, ServerSentEvent]`, over a
    * `StreamBackend[Task, ZioStreams]`; the implementation is the API in `Task`.
    */
  case Zio

  /** Streamed bodies as `Array[Byte]`, events as the `List[ServerSentEvent]` of a response read whole, over any
    * `Backend[F]`.
    */
  case Disabled
}

object Streaming {
  def parse(value: String): Either[String, Streaming] = value.toLowerCase match {
    case "fs2" => Right(Fs2)
    case "zio" => Right(Zio)
    case "none" => Right(Disabled)
    case other => Left(s"okapi.client.streaming must be fs2, zio or none, not '$other'")
  }
}

/** The sbt module of the client. `organization` and `version` are left out of `build.sbt` when `None`. */
final case class Module(name: String, organization: Option[String], version: Option[String], scalaVersion: String)

/** The packages of a generated client: the API traits in `root`, the models and the sttp implementation in packages
  * nested in it.
  */
final case class Packages(root: PackageName, models: PackageName, impl: PackageName)

/** What the generator writes, and where.
  *
  * @param directory
  *   the client module's directory, holding its `build.sbt`.
  * @param sourceDirectory
  *   the source root the packages are written under.
  * @param api
  *   the root trait; its sttp implementation is `Sttp<api>`.
  * @param separateModels
  *   a file per model (a sealed trait shares its file with its cases), else one `Models.scala`.
  * @param clean
  *   deletes the directory of [[Packages.root]] under `sourceDirectory` before writing.
  */
final case class Settings(
  directory: File,
  sourceDirectory: File,
  packages: Packages,
  api: Identifier,
  title: String,
  module: Module,
  buildFile: BuildFile,
  grouping: Grouping,
  streaming: Streaming,
  separateModels: Boolean,
  clean: Boolean,
)

object Settings {

  private def subPackage(root: PackageName, name: String): Either[String, PackageName] =
    PackageName(name).map(sub => root.sub(sub.value))

  /** Reads the settings from a properties file of `okapi.client.*` keys.
    *
    * @return
    *   the settings, or which key is missing or invalid.
    */
  def load(file: File): Either[String, Settings] = {
    val properties = new Properties()
    Using.resource(new FileInputStream(file))(properties.load)
    def optional(key: String) = Option(properties.getProperty(s"okapi.client.$key")).map(_.trim).filter(_.nonEmpty)
    def required(key: String) = optional(key).toRight(s"okapi.client.$key is missing from $file")
    def flag(key: String, default: Boolean) = optional(key).fold(default)(_.toBoolean)
    for {
      directory <- required("directory").map(new File(_))
      root <- required("package").flatMap(PackageName(_))
      models <- subPackage(root, optional("modelsPackage").getOrElse("models"))
      impl <- subPackage(root, optional("implPackage").getOrElse("impl"))
      api <- required("api").map(Identifier.tpeOrConverted)
      name <- required("name")
      scalaVersion <- required("scalaVersion")
      streaming <- Streaming.parse(optional("streaming").getOrElse("fs2"))
      buildFile <- optional("buildFile").fold(Right(BuildFile.IfMissing)) { value =>
        BuildFile.values
          .find(_.toString.equalsIgnoreCase(value.replace("-", "")))
          .toRight(s"okapi.client.buildFile must be one of ${BuildFile.values.mkString(", ")}, not '$value'")
      }
    } yield Settings(
      directory = directory,
      sourceDirectory = optional("sourceDirectory").fold(new File(directory, "src/main/scala"))(new File(_)),
      packages = Packages(root, models, impl),
      api = api,
      title = optional("title").getOrElse(api.bare),
      module = Module(name, optional("organization"), optional("version"), scalaVersion),
      buildFile = buildFile,
      grouping =
        if (flag("splitByController", default = true))
          Grouping.ByController(optional("controllerSuffix").getOrElse(Grouping.DefaultSuffix))
        else Grouping.Single,
      streaming = streaming,
      separateModels = flag("separateModels", default = true),
      clean = flag("clean", default = true),
    )
  }
}
