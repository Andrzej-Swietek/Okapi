import OkapiBuild.*

name := "okapi-client"
description := "Type-safe HTTP clients for Okapi API traits, for any effect."

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-sttp-client4" % V.tapir,
  "com.softwaremill.sttp.tapir" %% "tapir-sttp-stub4-server" % V.tapir % Test,
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % V.jsoniter % Test,
)

compilerSettings
zioTestSettings
publishSettings
