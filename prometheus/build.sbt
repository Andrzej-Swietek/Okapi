import OkapiBuild.*

name := "okapi-prometheus"
description := "Prometheus metrics for Okapi endpoints, for any effect: request counts, durations and active requests."

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-prometheus-metrics" % V.tapir
)

compilerSettings
zioJsonScheme
zioTestSettings
publishSettings
