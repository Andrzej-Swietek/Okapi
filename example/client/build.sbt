name := "example-client"
organization := "io.okapi.example"
version := "0.1.0-SNAPSHOT"
scalaVersion := "3.6.4"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core" % "4.0.26",
  "com.softwaremill.sttp.client4" %% "zio" % "4.0.26",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.41.2",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.41.2" % "compile-internal",
)

Compile / mainClass := Some("io.okapi.exampleclient.Main")
