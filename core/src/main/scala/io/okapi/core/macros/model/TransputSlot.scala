package io.okapi.core.macros.model

/** Which accumulator of an endpoint a transput is appended to, and the [[io.okapi.core.OkapiRuntime]] method doing it.
  */
private[okapi] enum TransputSlot(val runtimeMethod: String) {
  case Input extends TransputSlot("addInput")
  case Output extends TransputSlot("addOutput")
  case ErrorOutput extends TransputSlot("addErrorOutput")
}
