package io.okapi

package object core {

  /** A WebSocket handler: the stream of messages from the client to the stream of messages sent back. */
  type WsPipe[In, Out] = zio.stream.ZStream[Any, Throwable, In] => zio.stream.ZStream[Any, Throwable, Out]
}
