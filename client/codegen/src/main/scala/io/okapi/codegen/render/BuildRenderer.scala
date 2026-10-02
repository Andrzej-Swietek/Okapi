package io.okapi.codegen.render

import io.okapi.codegen.*
import StreamingSyntax.*

/** Renders the client module's `build.sbt`: sttp client4 (with its fs2 or zio module when streaming) and
  * jsoniter-scala, its macros at compile time only.
  */
private[codegen] object BuildRenderer {

  def render(module: Module, streaming: Streaming): String = {
    val coordinates = List(
      Some(s"name := ${Literal(module.name)}"),
      module.organization.map(o => s"organization := ${Literal(o)}"),
      module.version.map(v => s"version := ${Literal(v)}"),
      Some(s"scalaVersion := ${Literal(module.scalaVersion)}"),
    ).flatten
    val dependencies = List(
      Some(s""""com.softwaremill.sttp.client4" %% "core" % "${OkapiVersions.sttpClient4}""""),
      streaming.dependency(OkapiVersions.sttpClient4),
      Some(s""""com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "${OkapiVersions.jsoniter}""""),
      Some(
        s""""com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "${OkapiVersions.jsoniter}" % "compile-internal""""
      ),
    ).flatten
    coordinates.mkString("", "\n", "\n\n") + dependencies
      .map("  " + _ + ",")
      .mkString("libraryDependencies ++= Seq(\n", "\n", "\n)\n")
  }
}
