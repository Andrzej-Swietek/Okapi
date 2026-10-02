ThisBuild / organization := "io.github.andrzej-swietek"
ThisBuild / version := "0.2.0-SNAPSHOT"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := "3.6.4"

/** Effect-agnostic macros and runtime (no ZIO effect dependency). */
lazy val core = project.in(file("core"))

/** OpenAPI documents and Swagger UI for any effect. */
lazy val okapiOpenapi = project.in(file("modules/openapi"))

/** Clients for Okapi API traits, any effect. */
lazy val okapiClient = project.in(file("client/derived")).dependsOn(core)

/** Generates a client module from an OpenAPI document; run by sbt-okapi on the API project's Test classpath. */
lazy val okapiCodegen = project.in(file("client/codegen"))

/** Compiles and tests the clients okapi-codegen's GenerateFixture writes, regenerated on every Test compile. */
lazy val okapiCodegenIt = project
  .in(file("client/codegen-it"))
  .settings(
    Test / sourceGenerators += Def.taskDyn {
      val out = (Test / sourceManaged).value / "client"
      Def.task {
        (okapiCodegen / Test / runMain).toTask(s""" io.okapi.codegen.GenerateFixture "${out.getAbsolutePath}"""").value
        (out ** "*.scala").get
      }
    }.taskValue
  )

/** sbt plugin: OpenAPI document and generated client module of an Okapi API. */
lazy val sbtOkapi = project.in(file("sbt"))

/** ZIO specialisation: ZIO HTTP, ZStream / WebSocket, ZLayer. */
lazy val okapiZio = project.in(file("modules/zio")).dependsOn(core, okapiOpenapi)

/** Endpoint metrics for any effect (callback, Prometheus); ZIO HTTP only in its tests. */
lazy val okapiMetrics = project.in(file("modules/metrics")).dependsOn(okapiZio % "test->compile")

lazy val root = (project in file("."))
  .aggregate(core, okapiOpenapi, okapiZio, okapiMetrics, okapiClient, okapiCodegen, okapiCodegenIt, sbtOkapi)
  .settings(
    publish / skip := true
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
