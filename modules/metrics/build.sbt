import OkapiBuild.*

name := "okapi-metrics"
description := "Metrics for Okapi endpoints, for any effect: a per-request callback and Prometheus metrics."

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-prometheus-metrics" % V.tapir
)

compilerSettings
zioTestSettings
publishSettings
