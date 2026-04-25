ThisBuild / scalaVersion := "3.6.4"

val zioVersion = "2.1.16"
val zioHttpVersion = "3.2.0"
val zioJsonVersion = "0.7.43"
val tapirVersion = "1.11.29"
val zioLoggingVersion = "2.5.0"

lazy val core = (project in file("."))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-http" % zioHttpVersion,
      "dev.zio" %% "zio-json" % zioJsonVersion,
      "dev.zio" %% "zio-logging" % zioLoggingVersion,
      "dev.zio" %% "zio-logging-slf4j" % zioLoggingVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-zio" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      "dev.zio" %% "zio-test-junit" % zioVersion % Test,
      "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
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
    Compile / doc / sources := Seq.empty,
    externalResolvers ++= Seq(
      Resolver.defaultLocal
    ),
    publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/Andrzej-Swietek/Okapi"),
    publishMavenStyle := true,
    credentials += Credentials(
      "GitHub Package Registry",
      "maven.pkg.github.com",
      "Andrzej-Swietek",
      sys.env.getOrElse("GITHUB_TOKEN", ""),
    ),
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
