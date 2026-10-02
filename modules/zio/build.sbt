import OkapiBuild.*

name := "okapi-zio"
description := "Okapi for ZIO: ZIO HTTP routes, ZStream / WebSocket bodies and ZLayer wiring on top of okapi-core."

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % V.zio,
  "dev.zio" %% "zio-http" % V.zioHttp,
  "dev.zio" %% "zio-logging" % V.zioLogging,
  "dev.zio" %% "zio-logging-slf4j" % V.zioLogging,
  "dev.zio" %% "zio-json" % V.zioJson,
  "com.softwaremill.sttp.tapir" %% "tapir-zio" % V.tapir,
  "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % V.tapir,
)

compilerSettings
zioTestSettings
publishSettings
