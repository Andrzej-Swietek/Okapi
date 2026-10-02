import OkapiBuild.*

name := "okapi-codegen"
description := "Generates a tagless-final sttp client module from an OpenAPI document."

libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-openapi-codegen-core" % V.tapir

compilerSettings
// the generator works on Java strings throughout; flexible types keep that interop free of `.nn`
scalacOptions -= "-Yno-flexible-types"
zioTestSettings
publishSettings

// the library versions a generated client module depends on
Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "io" / "okapi" / "codegen" / "OkapiVersions.scala"
  IO.write(
    file,
    s"""package io.okapi.codegen
       |
       |private[codegen] object OkapiVersions {
       |  val sttpClient4 = "${V.sttpClient4}"
       |  val jsoniter = "${V.jsoniter}"
       |}
       |""".stripMargin,
  )
  Seq(file)
}.taskValue
