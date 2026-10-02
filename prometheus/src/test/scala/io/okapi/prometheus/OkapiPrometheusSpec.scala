package io.okapi.prometheus

import zio.{ Task, UIO, ZIO, ZLayer }
import zio.http.{ Request, URL }
import zio.test.*

import io.okapi.core.Okapi
import io.okapi.core.annotations.{ ApiTag, Controller, Get, Path }
import io.prometheus.metrics.model.registry.PrometheusRegistry
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }

object OkapiPrometheusSpec extends ZIOSpecDefault {

  @Controller("/metered")
  @ApiTag("Metered")
  final class MeteredController {

    @Get("/{id}")
    def get(@Path("id") id: Int): UIO[String] = ZIO.succeed(s"item $id")
  }

  override def spec = {
    test("requests are counted per method, route template, controller and status class") {
      val metrics = OkapiPrometheus[Task](registry = new PrometheusRegistry())
      val options =
        ZioHttpServerOptions.customiseInterceptors[Any].metricsInterceptor(metrics.metricsInterceptor()).options
      val routes = Okapi.httpRoutes[MeteredController](options) ++ ZioHttpInterpreter().toHttp(metrics.metricsEndpoint)
      def get(path: String) =
        ZIO.scoped(routes.runZIO(Request.get(URL.decode(path).toOption.get)).flatMap(_.body.asString))

      (for {
        _ <- get("/metered/1")
        _ <- get("/metered/2")
        scrape <- get("/metrics")
      } yield assertTrue(
        scrape.contains(
          """okapi_request_total{controller="Metered",method="GET",path="/metered/{id}",status="2xx"} 2"""
        ),
        scrape.contains("okapi_request_duration_seconds"),
      )).provide(ZLayer.succeed(MeteredController()))
    }
  }
}
