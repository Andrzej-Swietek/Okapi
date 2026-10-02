ThisBuild / organization := "io.okapi.example"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.4"

val okapiVersion = "0.2.0"
val zioVersion = "2.1.26"
val zioHttpVersion = "3.11.6"
val zioJsonVersion = "1.0.0"
val tapirVersion = "1.13.32"
val zioLoggingVersion = "2.5.3"
val zioConfigVersion = "4.0.8"

lazy val example = (project in file("."))
  .enablePlugins(OkapiPlugin)
  .settings(
    okapiSpec := "io.okapi.exampleApp.ApiSpec.yaml",
    okapiClientName := "example-client",
    okapiClientPackage := "io.okapi.exampleclient.api",
    okapiClientApi := "ExampleApi",
    okapiClientStreaming := "zio",
    okapiClientTitle := "Okapi Example API",
    okapiClientDirectory := baseDirectory.value.getParentFile / "client",
    name := "okapi-example",
    // zio-http needs zio-json 1.x; tapir-json-zio is built against 0.10
    libraryDependencySchemes += "dev.zio" %% "zio-json" % VersionScheme.Always,
    Compile / mainClass := Some("io.okapi.exampleApp.Main"),
    Compile / discoveredMainClasses := Seq("io.okapi.exampleApp.Main"),
    libraryDependencies ++= Seq(
      "io.github.andrzej-swietek" %% "okapi-zio" % okapiVersion,
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-http" % zioHttpVersion,
      "dev.zio" %% "zio-json" % zioJsonVersion,
      "dev.zio" %% "zio-logging" % zioLoggingVersion,
      "dev.zio" %% "zio-config" % zioConfigVersion,
      "dev.zio" %% "zio-config-typesafe" % zioConfigVersion,
      "dev.zio" %% "zio-logging-slf4j" % zioLoggingVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-zio" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion
    ),
    resolvers ++= Seq(
      Resolver.defaultLocal,
      Resolver.mavenLocal
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines:128",
      "-Yexplicit-nulls",
      "-Yno-flexible-types",
      "-Wsafe-init",
      "-Wunused:all",
      "-Wnonunit-statement",
      "-explain",
      "-explain-types",
      "-no-indent"
    )
  )
