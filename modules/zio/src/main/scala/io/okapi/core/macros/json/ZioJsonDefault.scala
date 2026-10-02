package io.okapi.core.macros.json

import io.okapi.core.OkapiZioRuntime
import io.okapi.core.macros.codecs.CodecSupport
import scala.quoted.*
import sttp.tapir.{ Codec, CodecFormat }

/** okapi-zio's last-resort JSON: a zio-json `JsonCodec[T]` (e.g. `derives JsonCodec`) with a summoned or derived
  * `Schema[T]`, used when neither a Tapir JSON codec nor a jsoniter-scala codec is in scope.
  */
private[okapi] trait ZioJsonDefault extends CodecSupport {

  override def fallbackJsonCodec[T: Type]: Option[Expr[Codec[String, T, CodecFormat.Json]]] = {
    super.fallbackJsonCodec[T].orElse {
      Expr.summon[zio.json.JsonCodec[T]].map { codec =>
        val tapirSchema = schema[T]
        '{ OkapiZioRuntime.zioJsonCodec[T]($codec, $tapirSchema) }
      }
    }
  }
}
