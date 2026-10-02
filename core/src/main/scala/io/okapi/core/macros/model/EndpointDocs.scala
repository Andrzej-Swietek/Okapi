package io.okapi.core.macros.model

/** OpenAPI metadata attached to every generated endpoint; `name` becomes the operation id. */
private[okapi] final case class EndpointDocs(
  name: String,
  tag: String,
  summary: Option[String],
  description: Option[String],
  deprecated: Boolean,
)
