package io.okapi.core.macros.model

/** Where an argument of an explicit parameter clause comes from: the n-th HTTP parameter, or the request body. */
private[okapi] enum ArgSlot {
  case Param(index: Int)
  case Body
}
