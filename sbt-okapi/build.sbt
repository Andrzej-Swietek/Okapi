import OkapiBuild.*

name := "sbt-okapi"
description := "sbt tasks writing an Okapi API's OpenAPI document and generating a client module from it."

sbtPlugin := true
scalaVersion := "2.12.20"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-openapi-codegen-core" % V.tapir,
  "dev.zio" %% "zio-test" % V.zio % Test,
  "dev.zio" %% "zio-test-sbt" % V.zio % Test,
)
testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

// the library versions a generated client module depends on
Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "io" / "okapi" / "sbt" / "OkapiVersions.scala"
  IO.write(
    file,
    s"""package io.okapi.sbt
       |
       |private[sbt] object OkapiVersions {
       |  val tapir = "${V.tapir}"
       |  val jsoniter = "${V.jsoniter}"
       |  val sttpShared = "${V.sttpShared}"
       |}
       |""".stripMargin,
  )
  Seq(file)
}.taskValue

publishSettings
