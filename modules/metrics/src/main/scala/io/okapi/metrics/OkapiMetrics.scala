package io.okapi.metrics

import java.time.{ Clock, Duration }

import sttp.tapir.AnyEndpoint
import sttp.tapir.server.interceptor.metrics.MetricsRequestInterceptor
import sttp.tapir.server.metrics.{ EndpointMetric, Metric }

/** One request served by an endpoint.
  *
  * @param path
  *   the route template, e.g. `/api/devices/{id}`
  * @param controller
  *   the endpoint's first tag, empty if it has none
  */
final case class RequestRecord(
  method: String,
  path: String,
  controller: String,
  status: Int,
  duration: Duration,
)

object OkapiMetrics {

  /** Calls `record` once per request that matched an endpoint, when its response body is complete; a failure of the
    * server logic is recorded with status 500. Add it to the server options with `metricsInterceptor`.
    */
  def interceptor[F[_]](
    record: RequestRecord => F[Unit],
    clock: Clock = Clock.systemUTC().nn,
  ): MetricsRequestInterceptor[F] =
    MetricsRequestInterceptor[F](List(metric(record, clock)), Seq.empty)

  private def metric[F[_]](record: RequestRecord => F[Unit], clock: Clock): Metric[F, Unit] = {
    Metric[F, Unit](
      (),
      (request, _, monad) => {
        monad.eval {
          val start = clock.instant().nn
          def done(endpoint: AnyEndpoint, status: Int): F[Unit] = {
            monad.suspend {
              record(
                RequestRecord(
                  method = request.method.method,
                  path = endpoint.showPathTemplate(showQueryParam = None),
                  controller = endpoint.info.tags.headOption.getOrElse(""),
                  status = status,
                  duration = Duration.between(start, clock.instant()).nn,
                )
              )
            }
          }
          EndpointMetric[F]().onResponseBody((endpoint, response) => done(endpoint, response.code.code)).onException {
            (endpoint, _) => done(endpoint, 500)
          }
        }
      },
    )
  }
}
