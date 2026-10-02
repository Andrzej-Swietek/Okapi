package io.okapi.core.macros.model

/** Where a controller-method parameter is read from, by its annotation. */
private[okapi] enum ParamKind(val annotation: OkapiAnnotation) {
  case Path extends ParamKind(OkapiAnnotation.Path)
  case Query extends ParamKind(OkapiAnnotation.Query)
  case Header extends ParamKind(OkapiAnnotation.Header)
  case Cookie extends ParamKind(OkapiAnnotation.Cookie)
  case BearerAuth extends ParamKind(OkapiAnnotation.BearerAuth)
}

private[okapi] object ParamKind {
  def fromAnnotation(annotation: OkapiAnnotation): Option[ParamKind] = values.find(_.annotation == annotation)
}
