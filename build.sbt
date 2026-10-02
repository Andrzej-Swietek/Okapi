ThisBuild / organization := "io.github.andrzej-swietek"
ThisBuild / version := "0.2.0-SNAPSHOT"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := "3.6.4"

/** Effect-agnostic macros and runtime (no ZIO effect dependency). */
lazy val core = project.in(file("core"))

/** OpenAPI documents and Swagger UI for any effect. */
lazy val okapiOpenapi = project.in(file("openapi"))

/** ZIO specialisation: ZIO HTTP, ZStream / WebSocket, ZLayer. */
lazy val okapiZio = project.in(file("zio")).dependsOn(core, okapiOpenapi)

/** Endpoint metrics for any effect (callback, Prometheus); ZIO HTTP only in its tests. */
lazy val okapiMetrics = project.in(file("metrics")).dependsOn(okapiZio % "test->compile")

lazy val root = (project in file("."))
  .aggregate(core, okapiOpenapi, okapiZio, okapiMetrics)
  .settings(
    publish / skip := true
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
