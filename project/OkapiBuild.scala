import sbt.*
import sbt.Keys.*
import xerial.sbt.Sonatype.autoImport.*
import xerial.sbt.Sonatype.sonatypeCentralHost

/** Versions and settings shared by the Okapi modules. */
object OkapiBuild {

  object V {
    val zio = "2.1.26"
    val zioHttp = "3.11.6"
    val zioJson = "1.0.0"
    val tapir = "1.13.32"
    val zioLogging = "2.5.3"
    val jsoniter = "2.41.2"
    val sttpClient4 = "4.0.26" // the sttp client4 version tapir depends on
    val scalafmt = "3.11.5"
  }

  lazy val compilerSettings: Seq[Setting[?]] = Seq(
    scalacOptions ++= Seq(
      "-Xmax-inlines:128",
      "-Yexplicit-nulls",
      "-Yno-flexible-types",
      "-Wsafe-init",
      "-Wunused:all",
      "-Wnonunit-statement",
      "-explain",
      "-explain-types",
      "-no-indent",
    ),
    // scaladoc fails on the macros; Central accepts the resulting near-empty javadoc jar
    Compile / doc / sources := Seq.empty,
    externalResolvers ++= Seq(Resolver.defaultLocal),
  )

  /** Lets tapir-json-zio, built against zio-json 0.10, resolve to zio-json 1.x in core's tests. */
  lazy val zioJsonScheme: Seq[Setting[?]] = Seq(
    libraryDependencySchemes += "dev.zio" %% "zio-json" % VersionScheme.Always
  )

  lazy val zioTestSettings: Seq[Setting[?]] = Seq(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test" % V.zio % Test,
      "dev.zio" %% "zio-test-sbt" % V.zio % Test,
      "dev.zio" %% "zio-test-junit" % V.zio % Test,
      "dev.zio" %% "zio-test-magnolia" % V.zio % Test,
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

  /** Publishing to Maven Central through the Sonatype Central Portal. */
  lazy val publishSettings: Seq[Setting[?]] = Seq(
    publishTo := sonatypePublishToBundle.value,
    publishMavenStyle := true,
    sonatypeCredentialHost := sonatypeCentralHost,
    sonatypeProfileName := "io.github.andrzej-swietek",
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/Andrzej-Swietek/Okapi")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/Andrzej-Swietek/Okapi"),
        "scm:git:https://github.com/Andrzej-Swietek/Okapi.git",
        "scm:git:git@github.com:Andrzej-Swietek/Okapi.git",
      )
    ),
    developers := List(
      Developer(
        id = "Andrzej-Swietek",
        name = "Andrzej Świętek",
        email = "a.swietek@avsystem.com",
        url = url("https://github.com/Andrzej-Swietek"),
      )
    ),
  )
}
