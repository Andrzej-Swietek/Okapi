package io.okapi.core.macros.support

import scala.quoted.*

/** Common base of every macro module.
  *
  * All modules of one macro expansion are mixed into a single instance, so they share one `Quotes` and their
  * path-dependent reflect types (`Term`, `TypeRepr`, `Symbol`) line up without casts.
  */
private[okapi] trait MacroContext {
  val q: Quotes
  given Quotes = q
  import q.reflect.*

  def abort(message: String): Nothing = report.errorAndAbort(message)

  /** `OkapiRuntime.<name>[typeArgs*](args*)` */
  def callRuntime(name: String, typeArgs: List[TypeRepr], args: List[Term]): Term =
    callModule("io.okapi.core.OkapiRuntime", name, typeArgs, args)

  /** `<module>.<name>[typeArgs*](args*)` for a top-level object given by its full name. */
  def callModule(module: String, name: String, typeArgs: List[TypeRepr], args: List[Term]): Term = {
    val method = Select.unique(Ref(Symbol.requiredModule(module)), name)
    Apply(if typeArgs.isEmpty then method else TypeApply(method, typeArgs.map(Inferred(_))), args)
  }

  def cast(term: Term, tpe: TypeRepr): Term =
    TypeApply(Select.unique(term, "asInstanceOf"), List(Inferred(tpe)))

  /** The bare type constructor of an applied type, e.g. `ZIO` out of `ZIO[Any, Any, Any]`. */
  def typeConstructor(applied: TypeRepr): TypeRepr = {
    applied match {
      case AppliedType(tc, _) => tc
      case other => abort(s"Expected an applied type, got: ${other.show}")
    }
  }
}
