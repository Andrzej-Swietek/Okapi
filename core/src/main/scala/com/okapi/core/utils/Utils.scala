package com.okapi.core.utils

import java.util.UUID

trait Utils {
  extension [T](nullable: T | Null) {
    def safeApply(foo: T => Unit): Unit = {
      nullable match {
        case value: T @unchecked => foo(value)
        case null                => ()
      }
    }
  }
}

object Utils {
  def randomUUID(): String = UUID.randomUUID().nn.toString
}
