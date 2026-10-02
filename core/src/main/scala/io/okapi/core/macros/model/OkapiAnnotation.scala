package io.okapi.core.macros.model

/** Every annotation the macros read (see `io.okapi.core.annotations`).
  *
  * Annotations are matched on the class's simple name, so e.g. `@java.lang.Deprecated` counts as `@Deprecated`.
  */
private[okapi] enum OkapiAnnotation {
  case Controller, Tag, ApiTag
  case Get, Post, Put, Delete, Patch, WebSocket
  case Path, Query, Header, Cookie, BearerAuth, RequestBody
  case Consumes, Produces, Summary, Description, Deprecated, Status

  def name: String = toString

  /** `@Name`, for messages. */
  def show: String = s"@$name"
}

private[okapi] object OkapiAnnotation {
  private val byName = values.map(a => a.name -> a).toMap

  def named(name: String): Option[OkapiAnnotation] = byName.get(name)
}
