package io.okapi.core.macros.layers

import zio.ZLayer

import io.okapi.core.macros.support.TypeLists
import scala.quoted.*

/** Expansion of `controllerLayers` / `autoLayer`: `ZLayer.make[A & B & ...](ZLayer.derive[X], ...)`.
  *
  * Which types get a derived layer is a [[DependencyResolution]] strategy.
  */
final private[okapi] class LayerMacro(val q: Quotes) extends TypeLists {
  import q.reflect.*

  /** @param derived
    *   types to build with `ZLayer.derive`, dependencies before dependants.
    * @param external
    *   dependencies that cannot be derived (traits, abstract classes, library types); they become the layer's input.
    */
  final case class Resolution(derived: List[TypeRepr], external: List[TypeRepr])

  /** Strategy: from the requested root types to the layers to derive. */
  sealed trait DependencyResolution {
    def resolve(roots: List[TypeRepr]): Resolution
  }

  /** Derives layers for exactly the requested types. */
  object ExplicitTypes extends DependencyResolution {
    def resolve(roots: List[TypeRepr]): Resolution = Resolution(roots, Nil)
  }

  /** Walks primary constructors transitively. Whatever cannot be derived — abstract types and library types — is left
    * to the caller as an input of the resulting layer.
    */
  object ConstructorGraph extends DependencyResolution {

    def resolve(roots: List[TypeRepr]): Resolution = {
      val visited = roots.foldLeft(Resolution(Nil, Nil))(visit)
      visited.copy(derived = visited.derived.reverse, external = visited.external.reverse)
    }

    private def visit(state: Resolution, tpe: TypeRepr): Resolution = {
      val dep = tpe.dealias
      val sym = dep.typeSymbol
      if (state.derived ++ state.external).exists(_ =:= dep) then state
      else if !isDerivable(sym) then state.copy(external = dep :: state.external)
      else {
        // marks `dep` before its dependencies, then moves it after them: dependencies first, cycles terminate
        val afterDeps = constructorDependencies(dep).foldLeft(state.copy(derived = dep :: state.derived))(visit)
        afterDeps.copy(derived = dep :: afterDeps.derived.filterNot(_ =:= dep))
      }
    }

    private def constructorDependencies(tpe: TypeRepr): List[TypeRepr] =
      constructorClauses(tpe).collect { case (params, false) => params }.flatten

    /** A concrete class outside the library namespaces `scala.`, `java.`, `javax.`, `zio.`, `sttp.`. */
    private def isDerivable(sym: Symbol): Boolean = {
      sym.isClassDef &&
      !sym.flags.is(Flags.Trait) &&
      !sym.flags.is(Flags.Abstract) &&
      !List("scala.", "java.", "javax.", "zio.", "sttp.").exists(sym.fullName.startsWith)
    }
  }

  /** `ZLayer[In, E, Roots]`: `In` is `Any` unless some dependency must be supplied, `E` is what the derived layers can
    * fail with (see [[derivationError]]).
    */
  def expand[Types: Type](resolution: DependencyResolution): Expr[Any] = {
    tupleMembers(TypeRepr.of[Types]) match {
      case Nil => '{ ZLayer.empty }
      case roots =>
        val Resolution(derived, external) = resolution.resolve(roots)
        val layers = Varargs(derived.map(derive))
        (intersection(external).asType, derivationError(derived).asType, intersection(roots).asType) match {
          case ('[in], '[err], '[out]) =>
            // inside the quote `derive` is not expanded yet and is typed ZLayer[_, Any, _]: `err` comes from derivationError
            val layer = {
              if external.isEmpty then '{ ZLayer.make[out].apply[Any]($layers*) }
              else '{ ZLayer.makeSome[in, out].apply[Any]($layers*) }
            }
            ascribe(layer, TypeRepr.of[ZLayer[in, err, out]])
        }
    }
  }

  /** The error type of `ZLayer.derive` over `types`, by ZIO's derivation rules: the union of each type's
    * `ZLayer.Derive.Scoped[R, E]` lifecycle error and of the `E` of every `ZLayer.Derive.Default` instance resolved for
    * a constructor parameter (e.g. `Config.Error` for a `Config`-backed one). `Nothing` if none.
    */
  private def derivationError(types: List[TypeRepr]): TypeRepr = {
    val errors =
      types.flatMap(t => lifecycleError(t).toList ++ constructorClauses(t).flatMap(_._1).flatMap(defaultError))
    distinct(errors.filterNot(_ =:= TypeRepr.of[Nothing]))
      .reduceLeftOption(OrType(_, _))
      .getOrElse(TypeRepr.of[Nothing])
  }

  private def lifecycleError(tpe: TypeRepr): Option[TypeRepr] = {
    tpe.baseType(TypeRepr.of[ZLayer.Derive.Scoped].typeSymbol) match {
      case AppliedType(_, List(_, error)) => Some(error)
      case _ => None
    }
  }

  private def defaultError(param: TypeRepr): Option[TypeRepr] = {
    param.asType match {
      case '[p] =>
        Expr.summon[ZLayer.Derive.Default.WithContext[?, ?, p]].map { default =>
          val tpe = default.asTerm.tpe.dealias
          tpe.select(tpe.typeSymbol.typeMember("E")).dealias
        }
    }
  }

  /** Primary-constructor parameter types per clause (with an "is implicit" flag), type arguments substituted. */
  private def constructorClauses(tpe: TypeRepr): List[(List[TypeRepr], Boolean)] = {
    def clauses(method: TypeRepr): List[(List[TypeRepr], Boolean)] = {
      method match {
        case mt @ MethodType(_, params, result) => (params, mt.isImplicit) :: clauses(result)
        case _ => Nil
      }
    }
    val ctor = tpe.typeSymbol.primaryConstructor
    if ctor == Symbol.noSymbol then Nil
    else {
      tpe.memberType(ctor) match {
        case poly: PolyType => clauses(poly.appliedTo(tpe.typeArgs))
        case other => clauses(other)
      }
    }
  }

  private def ascribe(layer: Expr[Any], tpe: TypeRepr): Expr[Any] =
    Typed(cast(layer.asTerm, tpe), Inferred(tpe)).asExpr

  private def derive(tpe: TypeRepr): Expr[ZLayer[?, Any, ?]] =
    tpe.asType match { case '[t] => '{ ZLayer.derive[t] }.asExprOf[ZLayer[?, Any, ?]] }
}
