package io.okapi

package object core {
  type WsPipe[In, Out] = zio.stream.ZStream[Any, Throwable, In] => zio.stream.ZStream[Any, Throwable, Out]
}
