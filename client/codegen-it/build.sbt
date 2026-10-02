import OkapiBuild.*

name := "okapi-codegen-it"
publish / skip := true

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client4" %% "core" % V.sttpClient4 % Test,
  "com.softwaremill.sttp.client4" %% "fs2" % V.sttpClient4 % Test,
  "com.softwaremill.sttp.client4" %% "zio" % V.sttpClient4 % Test,
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % V.jsoniter % Test,
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % V.jsoniter % Test,
)

zioTestSettings
