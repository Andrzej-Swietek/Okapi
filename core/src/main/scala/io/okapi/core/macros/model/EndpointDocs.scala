package io.okapi.core.macros.model

/** OpenAPI metadata attached to every generated endpoint. */
final private[okapi] case class EndpointDocs(
  tag: String,
  summary: Option[String],
  description: Option[String],
  deprecated: Boolean,
)
