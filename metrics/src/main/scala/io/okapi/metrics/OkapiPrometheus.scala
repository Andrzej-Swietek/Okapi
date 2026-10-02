package io.okapi.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import sttp.tapir.server.metrics.MetricLabels
import sttp.tapir.server.metrics.prometheus.PrometheusMetrics

/** Prometheus metrics for Okapi endpoints in any effect `F`:
  *   - `<namespace>_request_active{method, path, controller}` (gauge)
  *   - `<namespace>_request_total{method, path, controller, status}` (counter)
  *   - `<namespace>_request_duration_seconds{method, path, controller, status, phase}` (histogram)
  *
  * `path` is the route template (`/api/books/{id}`), `controller` the controller's tag (`@Tag` / `@ApiTag`, else its
  * class name) and `status` the status class (`2xx`, `4xx`, ...). Add [[PrometheusMetrics.metricsInterceptor]] to the
  * server options and serve [[PrometheusMetrics.metricsEndpoint]] (`GET /metrics`) next to the routes.
  */
object OkapiPrometheus {

  val Labels: MetricLabels = {
    val controller: (String, sttp.tapir.AnyEndpoint => String) = "controller" -> (_.info.tags.headOption.getOrElse(""))
    MetricLabels.Default.copy(forEndpoint = MetricLabels.Default.forEndpoint :+ controller)
  }

  def apply[F[_]](
    namespace: String = "okapi",
    registry: PrometheusRegistry = PrometheusRegistry.defaultRegistry.nn,
  ): PrometheusMetrics[F] =
    PrometheusMetrics.default[F](namespace, registry, Labels)
}
