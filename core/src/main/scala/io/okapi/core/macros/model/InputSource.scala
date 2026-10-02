package io.okapi.core.macros.model

/** What a request input carries to the controller: nothing (a fixed path segment), the n-th HTTP parameter, or the
  * request body.
  */
private[okapi] enum InputSource {
  case Fixed
  case Param(index: Int)
  case Body
}
