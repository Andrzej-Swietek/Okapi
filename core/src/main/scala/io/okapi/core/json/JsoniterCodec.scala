package io.okapi.core.json

import com.github.plokhotnyuk.jsoniter_scala.core.{ JsonReader, JsonValueCodec, JsonWriter }
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }
import scala.annotation.nowarn
import scala.compiletime.summonInline
import scala.deriving.Mirror
import sttp.tapir.Schema
import sttp.tapir.generic.Configuration

/** A jsoniter-scala codec together with the Tapir schema documenting the same JSON, derivable with `derives`:
  * {{{
  * final case class Book(id: Int, title: String) derives JsoniterCodec
  *
  * sealed trait Shape derives JsoniterCodec            // {"type": "Circle", "radius": 1.0}
  * final case class Circle(radius: Double) extends Shape
  * case object Dot extends Shape                       // {"type": "Dot"}
  *
  * enum Color derives JsoniterCodec { case Red, Green } // {"type": "Red"}
  * }}}
  * Sealed hierarchies and enums carry their case name in the [[JsoniterCodec.Discriminator]] field, in the codec and in
  * the schema alike, so the OpenAPI docs describe exactly what is sent. It is a `JsonValueCodec`, so it works wherever
  * one is expected; Okapi picks up both its codec and its schema.
  */
trait JsoniterCodec[A] extends JsonValueCodec[A] {
  def schema: Schema[A]
}

object JsoniterCodec {

  /** The field naming the case of a sealed hierarchy or enum. */
  final val Discriminator = "type"

  /** Called by `derives JsoniterCodec`. `inline`, so the derivation macros expand for the concrete `A`. */
  inline def derived[A]: JsoniterCodec[A] =
    from(JsonCodecMaker.make[A](CodecMakerConfig.withDiscriminatorFieldName(Some(Discriminator))), schemaOf[A])

  /** The schema matching [[derived]]'s JSON: generic derivation (nested types included) with [[Discriminator]]. An
    * explicit `given Schema` for a nested type still takes precedence.
    */
  @nowarn("msg=unused import") // used where the body is inlined: nested types' schemas derive from it
  inline def schemaOf[A]: Schema[A] = {
    import sttp.tapir.generic.auto.*
    given Configuration = Configuration.default.withDiscriminator(Discriminator)
    Schema.derived[A](using summon[Configuration], summonInline[Mirror.Of[A]])
  }

  def from[A](codec: JsonValueCodec[A], tapirSchema: Schema[A]): JsoniterCodec[A] = {
    new JsoniterCodec[A] {
      val schema: Schema[A] = tapirSchema
      def decodeValue(in: JsonReader, default: A): A = codec.decodeValue(in, default)
      def encodeValue(x: A, out: JsonWriter): Unit = codec.encodeValue(x, out)
      def nullValue: A = codec.nullValue
    }
  }
}
