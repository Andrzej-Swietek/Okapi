package io.okapi.core.macros.model

/** HTTP verbs, by their routing annotation. */
private[okapi] enum HttpVerb(val annotation: OkapiAnnotation) {
  case Get extends HttpVerb(OkapiAnnotation.Get)
  case Post extends HttpVerb(OkapiAnnotation.Post)
  case Put extends HttpVerb(OkapiAnnotation.Put)
  case Delete extends HttpVerb(OkapiAnnotation.Delete)
  case Patch extends HttpVerb(OkapiAnnotation.Patch)
}

private[okapi] object HttpVerb {
  def fromAnnotation(annotation: OkapiAnnotation): Option[HttpVerb] = values.find(_.annotation == annotation)
}
