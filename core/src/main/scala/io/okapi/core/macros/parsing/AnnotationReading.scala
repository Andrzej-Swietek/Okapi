package io.okapi.core.macros.parsing

import io.okapi.core.macros.model.OkapiAnnotation
import io.okapi.core.macros.support.MacroContext

/** Reads Okapi annotations (matched by simple name) and their constant arguments off symbols.
  *
  * Annotations are inherited, nearest first: a method from the methods it overrides, a parameter from the parameter at
  * the same position in those methods, a class from its base classes. An annotated API trait can therefore be
  * implemented by an unannotated class.
  */
private[okapi] trait AnnotationReading extends MacroContext {
  import q.reflect.*

  extension (sym: Symbol) {
    def findAnnotation(annotation: OkapiAnnotation): Option[Term] =
      sym.nearest(_.annotations.find(okapiAnnotation(_).contains(annotation)))

    /** The Okapi annotations placed directly on the symbol (not inherited). */
    def okapiAnnotations: List[(OkapiAnnotation, Term)] = sym.annotations.flatMap(a => okapiAnnotation(a).map(_ -> a))

    /** The first defined result of `f` along the symbol's annotation lineage. */
    def nearest[A](f: Symbol => Option[A]): Option[A] = lineage(sym).iterator.map(f).collectFirst { case Some(a) => a }

    def isAnnotatedWith(annotation: OkapiAnnotation): Boolean = findAnnotation(annotation).isDefined

    /** The string argument of `@name("...")`; `Some("")` when the annotation relies on its default argument. */
    def annotationArg(annotation: OkapiAnnotation): Option[String] =
      findAnnotation(annotation).map(stringArg(_).getOrElse(""))

    /** Like [[annotationArg]], but treats an empty argument as absent. */
    def nonEmptyAnnotationArg(annotation: OkapiAnnotation): Option[String] =
      annotationArg(annotation).filter(_.nonEmpty)

    def intAnnotationArg(annotation: OkapiAnnotation): Option[Int] = findAnnotation(annotation).flatMap(intArg)
  }

  def okapiAnnotation(annotation: Term): Option[OkapiAnnotation] = OkapiAnnotation.named(annotation.tpe.typeSymbol.name)

  /** `sym` followed by the symbols it inherits annotations from. */
  private def lineage(sym: Symbol): List[Symbol] = {
    if sym.isClassDef then sym.typeRef.baseClasses
    else if sym.isDefDef then sym :: sym.allOverriddenSymbols.toList
    else if sym.isTerm && sym.owner.isDefDef then {
      val position = sym.owner.paramSymss.flatten.indexOf(sym)
      sym :: sym.owner.allOverriddenSymbols.toList.flatMap(_.paramSymss.flatten.lift(position))
    }
    else List(sym)
  }

  /** The constant string of an annotation's first argument, `None` when it relies on its default. */
  def stringArg(annotation: Term): Option[String] =
    literalArg(annotation, firstParam(annotation), "a string") { case StringConstant(v) => v }

  private def intArg(annotation: Term): Option[Int] =
    literalArg(annotation, firstParam(annotation), "an Int") { case IntConstant(v) => v }

  /** The constant Int given to the annotation's constructor parameter `param`; `None` when it relies on its default. */
  def intParam(annotation: Term, param: String): Option[Int] =
    literalArg(annotation, param, "an Int") { case IntConstant(v) => v }

  /** Accepts `@A(lit)`, `@A(name = lit)`, a constant (`final val P = "/x"`, `@A(P)`) and `@A()` / `@A` (default
    * argument → `None`). Anything else, e.g. a non-final `val`, is a compile error at the argument.
    */
  private def literalArg[A](annotation: Term, param: String, expected: String)(read: PartialFunction[Constant, A])
    : Option[A] =
    argument(annotation, param).flatMap(constantArg(annotation, _, expected)(read))

  /** The argument passed for constructor parameter `param`: by name, or at that parameter's position. */
  private def argument(annotation: Term, param: String): Option[Term] = {
    annotation match {
      case Apply(_, args) =>
        val position = constructorParams(annotation).indexOf(param)
        args
          .collectFirst { case NamedArg(`param`, value) => value }
          .orElse(args.lift(position).filter { case _: NamedArg => false; case _ => true })
      case other => abort(s"Unexpected annotation shape: ${other.show}")
    }
  }

  private def constructorParams(annotation: Term): List[String] =
    annotation.tpe.typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTerm).map(_.name)

  private def firstParam(annotation: Term): String = constructorParams(annotation).headOption.getOrElse("")

  /** The constant `arg` of `annotation` read with `read`; `None` for a default argument, a compile error otherwise. */
  private def constantArg[A](annotation: Term, arg: Term, expected: String)(read: PartialFunction[Constant, A])
    : Option[A] = {
    unwrap(arg) match {
      case ConstantValue(constant) if read.isDefinedAt(constant) => Some(read(constant))
      case DefaultArgument() => None
      case other =>
        report.errorAndAbort(
          s"@${annotation.tpe.typeSymbol.name} expects $expected literal or constant argument, got: ${other.show}",
          arg.pos,
        )
    }
  }

  /** A reference to a default-argument getter (`$lessinit$greater$default$1`): the annotation's own default. */
  private object DefaultArgument {
    def unapply(term: Term): Boolean = {
      term match {
        case Ident(name) => name.contains("$default$")
        case Select(_, name) => name.contains("$default$")
        case _ => false
      }
    }
  }

  /** A literal, or a reference whose type is a constant type (e.g. a `final val` with a literal right-hand side). */
  private object ConstantValue {
    def unapply(term: Term): Option[Constant] = {
      term.tpe.widenTermRefByName.dealias match {
        case ConstantType(constant) => Some(constant)
        case _ => None
      }
    }
  }

  private def unwrap(arg: Term): Term = {
    arg match {
      case NamedArg(_, value) => unwrap(value)
      case Typed(value, _) => unwrap(value)
      case Inlined(_, Nil, value) => unwrap(value)
      case other => other
    }
  }
}
