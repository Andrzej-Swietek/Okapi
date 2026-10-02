package io.okapi.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import sttp.tapir.server.metrics.MetricLabels
import sttp.tapir.server.metrics.prometheus.PrometheusMetrics

/** Prometheus metrics for Tapir endpoints in any effect `F`:
  *   - `<namespace>_request_active{method, path, controller}` (gauge)
  *   - `<namespace>_request_total{method, path, controller, status}` (counter)
  *   - `<namespace>_request_duration_seconds{method, path, controller, status, phase}` (histogram)
  *
  * `path` is the route template (`/api/books/{id}`), `controller` the endpoint's first tag (empty if it has none) and
  * `status` the status class (`2xx`, `4xx`, ...). Add [[PrometheusMetrics.metricsInterceptor]] to the server options
  * and serve [[PrometheusMetrics.metricsEndpoint]] (`GET /metrics`) next to the routes.
  */
object OkapiPrometheus {

  /** Tapir's default labels plus `controller`. */
  val Labels: MetricLabels = {
    val controller: (String, sttp.tapir.AnyEndpoint => String) = "controller" -> (_.info.tags.headOption.getOrElse(""))
    MetricLabels.Default.copy(forEndpoint = MetricLabels.Default.forEndpoint :+ controller)
  }

  /** Tapir's default Prometheus metrics with [[Labels]], registered in `registry`. */
  def apply[F[_]](
    namespace: String = "okapi",
    registry: PrometheusRegistry = PrometheusRegistry.defaultRegistry.nn,
  ): PrometheusMetrics[F] =
    PrometheusMetrics.default[F](namespace, registry, Labels)
}
