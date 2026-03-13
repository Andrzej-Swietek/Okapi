package com.okapi.core

import scala.quoted.Type

import zio.http.{ Response, Routes }

import sttp.tapir.ztapir.ZServerEndpoint

object Okapi {

  inline def endpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    macros.AnnotationProcessor.endpoints[T]

  inline def generateEndpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    macros.AnnotationProcessor.generateEndpoints[T]

  inline def swaggerRoutes[T](
    title: String,
    version: String,
  ): Routes[Any, Response] =
    macros.AnnotationProcessor.swaggerRoutes[T](title, version)

  inline def httpRoutes[T]: Routes[T, Response] =
    macros.AnnotationProcessor.httpRoutes[T]
}
