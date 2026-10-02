import OkapiBuild.*

name := "okapi-core"
description := "Okapi core: effect-agnostic, annotation-driven Tapir endpoints powered by Scala 3 macros."

// JSON: jsoniter-scala by default; any Tapir JSON integration in scope takes precedence (see "JSON" in OKAPI.md)
libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-core" % V.tapir,
  "com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % V.tapir,
  // `derives JsoniterCodec` expands JsonCodecMaker in user code, so the macros are a regular dependency
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % V.jsoniter,
  // tests also exercise swapping in another integration
  "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % V.tapir % Test,
)

compilerSettings
zioJsonScheme
zioTestSettings
publishSettings
