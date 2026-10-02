package io.okapi.core.macros.support

/** Type-level lists for macros working on several types: tuples of controller types and intersections of environments.
  */
private[okapi] trait TypeLists extends MacroContext {
  import q.reflect.*

  /** `(A, B, C)` / `A *: B *: EmptyTuple` → `List(A, B, C)`. */
  def tupleMembers(tpe: TypeRepr): List[TypeRepr] = {
    tpe.dealias match {
      case applied @ AppliedType(_, List(head, tail)) if applied.typeSymbol.fullName == "scala.*:" =>
        head :: tupleMembers(tail)
      case applied @ AppliedType(_, args) if applied.typeSymbol.fullName.startsWith("scala.Tuple") => args
      case empty if empty =:= TypeRepr.of[EmptyTuple] => Nil
      case other => abort(s"Expected a tuple of types, got: ${other.show}")
    }
  }

  /** `A & (B & Any)` → `List(A, B)`. */
  def conjuncts(tpe: TypeRepr): List[TypeRepr] = {
    tpe.dealias match {
      case AndType(left, right) => conjuncts(left) ++ conjuncts(right)
      case any if any =:= TypeRepr.of[Any] => Nil
      case other => List(other)
    }
  }

  /** Intersection of the given types without duplicates; `Any` for none. */
  def intersection(types: List[TypeRepr]): TypeRepr =
    distinct(types.flatMap(conjuncts)).reduceLeftOption(AndType(_, _)).getOrElse(TypeRepr.of[Any])

  def distinct(types: List[TypeRepr]): List[TypeRepr] =
    types.foldLeft(List.empty[TypeRepr])((acc, t) => if acc.exists(_ =:= t) then acc else acc :+ t)
}
