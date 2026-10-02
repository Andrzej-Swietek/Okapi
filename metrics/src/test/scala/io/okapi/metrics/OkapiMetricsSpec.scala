package io.okapi.metrics

import zio.{ Ref, Task, UIO, ZIO, ZLayer }
import zio.http.{ Handler, HandlerAspect, Request, Response, URL }
import zio.test.*

import io.okapi.core.Okapi
import io.okapi.core.annotations.{ ApiTag, Controller, Get, Path }
import io.prometheus.metrics.model.registry.PrometheusRegistry
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }

object OkapiMetricsSpec extends ZIOSpecDefault {

  @Controller("/metered")
  @ApiTag("Metered")
  final class MeteredController {

    @Get("/{id}")
    def get(@Path("id") id: Int): UIO[String] = ZIO.succeed(s"item $id")
  }

  /** A service a zio-http middleware needs from the environment. */
  final class RequestLog(val paths: Ref[List[String]])

  private def get[R](routes: zio.http.Routes[R, Response], path: String) =
    ZIO.scoped(routes.runZIO(Request.get(URL.decode(path).toOption.get)).flatMap(_.body.asString))

  override def spec = {
    suite("OkapiMetricsSpec")(
      test("Prometheus: requests counted per method, route template, controller and status class") {
        val metrics = OkapiPrometheus[Task](registry = new PrometheusRegistry())
        val options =
          ZioHttpServerOptions.customiseInterceptors[Any].metricsInterceptor(metrics.metricsInterceptor()).options
        val routes =
          Okapi.httpRoutes[MeteredController](options) ++ ZioHttpInterpreter().toHttp(metrics.metricsEndpoint)
        (for {
          _ <- get(routes, "/metered/1")
          _ <- get(routes, "/metered/2")
          scrape <- get(routes, "/metrics")
        } yield assertTrue(
          scrape.contains(
            """okapi_request_total{controller="Metered",method="GET",path="/metered/{id}",status="2xx"} 2"""
          ),
          scrape.contains("okapi_request_duration_seconds"),
        )).provide(ZLayer.succeed(MeteredController()))
      },
      test("callback: one record per request, with the route template") {
        for {
          records <- Ref.make(List.empty[RequestRecord])
          options = ZioHttpServerOptions
            .customiseInterceptors[Any]
            .metricsInterceptor(OkapiMetrics.interceptor[Task](r => records.update(_ :+ r)))
            .options
          _ <- get(Okapi.httpRoutes[MeteredController](options), "/metered/7")
            .provide(ZLayer.succeed(MeteredController()))
          recorded <- records.get
        } yield assertTrue(
          recorded.map(r => (r.method, r.path, r.controller, r.status)) == List(
            ("GET", "/metered/{id}", "Metered", 200)
          ),
          recorded.forall(!_.duration.isNegative),
        )
      },
      test("a zio-http middleware needing the environment applies to Okapi routes") {
        val logging: HandlerAspect[RequestLog, Unit] = HandlerAspect.interceptIncomingHandler(
          Handler.fromFunctionZIO[Request] { request =>
            ZIO.serviceWithZIO[RequestLog](_.paths.update(_ :+ request.url.path.toString)).as((request, ()))
          }
        )
        for {
          paths <- Ref.make(List.empty[String])
          body <- get(Okapi.httpRoutes[MeteredController] @@ logging, "/metered/3")
            .provide(ZLayer.succeed(MeteredController()), ZLayer.succeed(RequestLog(paths)))
          logged <- paths.get
        } yield assertTrue(body == "item 3", logged == List("/metered/3"))
      },
    )
  }
}
