name := "okapi-example-client"
scalaVersion := "3.6.4"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-core" % "1.13.32",
  "com.softwaremill.sttp.tapir" %% "tapir-sttp-client4" % "1.13.32",
  "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % "1.13.32",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.41.2",
  "com.softwaremill.sttp.shared" %% "zio" % "1.5.2",
)
