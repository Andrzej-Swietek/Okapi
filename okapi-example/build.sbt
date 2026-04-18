ThisBuild / organization := "io.okapi.example"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.4"

val okapiVersion = "0.1.1"
val zioVersion = "2.1.16"
val zioHttpVersion = "3.2.0"
val zioJsonVersion = "0.7.43"
val tapirVersion = "1.11.29"
val zioLoggingVersion = "2.5.0"
val zioConfigVersion = "4.0.0"

lazy val example = (project in file("."))
  .settings(
    name := "okapi-example",
    Compile / mainClass := Some("io.okapi.exampleApp.Main"),
    Compile / discoveredMainClasses := Seq("io.okapi.exampleApp.Main"),
    libraryDependencies ++= Seq(
      "io.okapi" %% "core" % okapiVersion,
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
