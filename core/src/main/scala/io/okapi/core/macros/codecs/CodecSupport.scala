package io.okapi.core.macros.codecs

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.okapi.core.OkapiRuntime
import io.okapi.core.json.JsoniterCodec
import io.okapi.core.macros.support.TypeShapes
import scala.quoted.*
import sttp.tapir.{ Codec, CodecFormat, EndpointIO, Schema }

/** Codec lookups and body transputs shared by request- and response-side codecs. */
private[okapi] trait CodecSupport extends TypeShapes {
  import q.reflect.*

  def summonOrAbort[C: Type](missing: => String): Expr[C] =
    Expr.summon[C].getOrElse(abort(missing))

  /** The JSON codec for `T`, first found of:
    *   1. a Tapir `Codec[String, T, CodecFormat.Json]` in scope at the expansion site — any Tapir JSON integration,
    *      e.g. `import sttp.tapir.json.circe.*`;
    *   1. jsoniter-scala (Okapi's default JSON library), see [[jsoniter]];
    *   1. the backend's [[fallbackJsonCodec]].
    *
    * `role` names the body in error messages.
    */
  def jsonCodec[T: Type](role: String): Expr[Codec[String, T, CodecFormat.Json]] = {
    summonUnambiguous[Codec[String, T, CodecFormat.Json]](role)
      .orElse(jsoniter[T](role).map((codec, schema) => '{ OkapiRuntime.jsoniterCodec[T]($codec, $schema) }))
      .orElse(fallbackJsonCodec[T])
      .getOrElse {
        abort(
          s"No JSON codec for ${TypeRepr.of[T].show} ($role). Add `derives JsoniterCodec` " +
            "(io.okapi.core.json.JsoniterCodec) or bring a Tapir JSON integration into scope."
        )
      }
  }

  /** A jsoniter codec for `T` and the schema documenting its JSON, first found of:
    *   1. a `JsoniterCodec[T]` (`derives JsoniterCodec`): its codec and its own, matching schema;
    *   1. a plain `JsonValueCodec[T]`: a summoned schema, or one derived like `JsoniterCodec.schemaOf`;
    *   1. a primitive, or a container — `Option`, `List`, `Seq`, `Vector`, `Set`, `Map[String, _]` — of such types: a
    *      codec made by `JsonCodecMaker.make` (reusing the elements' codecs), the primitive's schema or one built from
    *      the elements'.
    */
  private def jsoniter[T: Type](role: String): Option[(Expr[JsonValueCodec[T]], Expr[Schema[T]])] = {
    summonUnambiguous[JsoniterCodec[T]](role)
      .map(codec => (codec, '{ $codec.schema }))
      .orElse(summonUnambiguous[JsonValueCodec[T]](role).map(codec => (codec, plainJsoniterSchema[T])))
      .orElse {
        primitiveSchema[T](role).orElse(containerSchema[T](role)).map(schema => ('{ JsonCodecMaker.make[T] }, schema))
      }
  }

  private def plainJsoniterSchema[T: Type]: Expr[Schema[T]] =
    Expr.summon[Schema[T]].getOrElse('{ JsoniterCodec.schemaOf[T] })

  private def primitiveSchema[T: Type](role: String): Option[Expr[Schema[T]]] = {
    Option.when(isPrimitive(TypeRepr.of[T])) {
      summonOrAbort[Schema[T]](s"Missing given sttp.tapir.Schema[${Type.show[T]}] for the $role")
    }
  }

  private def containerSchema[T: Type](role: String): Option[Expr[Schema[T]]] = {
    def element[E: Type](build: Expr[Schema[E]] => Expr[Any]): Option[Expr[Schema[T]]] =
      jsoniter[E](role).map(found => build(found._2).asExprOf[Schema[T]])
    Type.of[T] match {
      case '[Option[e]] => element[e](s => '{ Schema.schemaForOption[e](using $s) })
      case '[List[e]] => element[e](s => '{ Schema.schemaForIterable[e, List](using $s) })
      case '[Vector[e]] => element[e](s => '{ Schema.schemaForIterable[e, Vector](using $s) })
      case '[Set[e]] => element[e](s => '{ Schema.schemaForIterable[e, Set](using $s) })
      case '[Seq[e]] => element[e](s => '{ Schema.schemaForIterable[e, Seq](using $s) })
      case '[Map[String, e]] => element[e](s => '{ Schema.schemaForMap[e](using $s) })
      case _ => None
    }
  }

  private def isPrimitive(tpe: TypeRepr): Boolean = {
    List(
      TypeRepr.of[String],
      TypeRepr.of[Int],
      TypeRepr.of[Long],
      TypeRepr.of[Short],
      TypeRepr.of[Byte],
      TypeRepr.of[Double],
      TypeRepr.of[Float],
      TypeRepr.of[Boolean],
      TypeRepr.of[Char],
      TypeRepr.of[BigDecimal],
      TypeRepr.of[BigInt],
      TypeRepr.of[java.util.UUID],
    ).exists(tpe.dealias =:= _)
  }

  /** Extension point: a backend's default JSON codec when neither a Tapir codec nor a jsoniter codec is in scope. */
  def fallbackJsonCodec[T: Type]: Option[Expr[Codec[String, T, CodecFormat.Json]]] = None

  def schema[T: Type]: Expr[Schema[T]] =
    summonOrDeriveSchema[T].getOrElse(abort(s"Missing given sttp.tapir.Schema[${TypeRepr.of[T].show}]"))

  /** `Expr.summon` that aborts on ambiguous instances. */
  private def summonUnambiguous[C: Type](role: String): Option[Expr[C]] = {
    Implicits.search(TypeRepr.of[C]) match {
      case success: ImplicitSearchSuccess => Some(success.tree.asExprOf[C])
      case ambiguous: AmbiguousImplicits => abort(s"Ambiguous ${TypeRepr.of[C].show} ($role): ${ambiguous.explanation}")
      case _ => None
    }
  }

  /** `String` body: `text/plain` by default, otherwise exactly the declared media type. */
  def stringBody(mediaType: Option[String]): Expr[EndpointIO.Body[String, String]] =
    mediaType.fold('{ sttp.tapir.stringBody })(m => '{ OkapiRuntime.textBody(${ Expr(m) }) })

  /** `Array[Byte]` body: `application/octet-stream` by default, otherwise exactly the declared media type. */
  def bytesBody(mediaType: Option[String]): Expr[EndpointIO.Body[Array[Byte], Array[Byte]]] =
    mediaType.fold('{ sttp.tapir.byteArrayBody })(m => '{ OkapiRuntime.binaryBody(${ Expr(m) }) })

  /** Whether a typed (non-`String`, non-binary) body with this media type is JSON: `application/json` or an
    * `application` subtype ending in `+json`, in any case and with any parameters.
    */
  def isJson(mediaType: String): Boolean = {
    sttp.model.MediaType.parse(mediaType).exists { m =>
      m.mainType == "application" && (m.subType == "json" || m.subType.endsWith("+json"))
    }
  }

  /** JSON body of `T`: `application/json` by default, otherwise exactly the declared media type. */
  def jsonBody[T: Type](role: String, mediaType: Option[String]): Expr[EndpointIO.Body[String, T]] = {
    val codec = jsonCodec[T](role)
    mediaType.fold('{ OkapiRuntime.jsonBody[T]($codec) })(m => '{ OkapiRuntime.jsonBody[T]($codec, ${ Expr(m) }) })
  }
}
