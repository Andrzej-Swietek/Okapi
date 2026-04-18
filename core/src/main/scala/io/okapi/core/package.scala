package io.okapi

import scala.collection.convert.AsScalaExtensions

package object core extends AsScalaExtensions with utils.Utils {
  type WsPipe[In, Out] = zio.stream.ZStream[Any, Throwable, In] => zio.stream.ZStream[Any, Throwable, Out]
}
