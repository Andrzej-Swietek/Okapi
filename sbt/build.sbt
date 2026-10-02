import OkapiBuild.*

name := "sbt-okapi"
description := "sbt tasks writing an Okapi API's OpenAPI document and generating a client module from it with okapi-codegen."

sbtPlugin := true
scalaVersion := "2.12.20"
scalacOptions ++= Seq("-Xlint:adapted-args", "-Xfatal-warnings")

libraryDependencies += "org.scalameta" %% "scalafmt-dynamic" % V.scalafmt

// the okapi-codegen the plugin adds to the API project, and the scalafmt formatting without a .scalafmt.conf
Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "io" / "okapi" / "sbt" / "OkapiVersions.scala"
  IO.write(
    file,
    s"""package io.okapi.sbt
       |
       |private[sbt] object OkapiVersions {
       |  val organization = "${organization.value}"
       |  val okapi = "${version.value}"
       |  val scalafmt = "${V.scalafmt}"
       |}
       |""".stripMargin,
  )
  Seq(file)
}.taskValue

publishSettings
