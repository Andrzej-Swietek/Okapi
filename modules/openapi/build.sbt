import OkapiBuild.*

name := "okapi-openapi"
description := "OpenAPI documents and Swagger UI endpoints for Okapi, for any effect."

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % V.tapir
)

compilerSettings
zioTestSettings
publishSettings
