package io.okapi.core.macros.parsing

import io.okapi.core.macros.model.{ ArgSlot, OkapiAnnotation, ParamKind }

/** Turns a controller method symbol into a [[MethodSpec]]: its HTTP parameters, body, media types and result. */
private[okapi] trait MethodParsing extends AnnotationReading {
  import q.reflect.*

  /** @param default
    *   the method's default-argument getter for this parameter: the parameter is then optional in the request, and the
    *   getter is called on the controller when it is absent (or empty, for [[Param.decodesAbsence]]) — the same value a
    *   Scala call would use.
    */
  final case class Param(name: String, tpe: TypeRepr, kind: ParamKind, default: Option[Symbol]) {

    /** An `Option` or a collection: absent from the request, it decodes as empty. */
    def decodesAbsence: Boolean = tpe <:< TypeRepr.of[Option[Any]] || tpe <:< TypeRepr.of[Iterable[Any]]

    /** The type Tapir decodes: `Option[T]` for a parameter with a default, unless [[decodesAbsence]]. */
    def inputType: TypeRepr =
      if default.isDefined && !decodesAbsence then TypeRepr.of[Option].appliedTo(tpe) else tpe
  }

  /** One term parameter clause of the method. An explicit clause lists where each argument comes from; an implicit /
    * `using` clause carries the arguments summoned at the expansion site.
    */
  final case class Clause(slots: List[ArgSlot], summoned: Option[List[Term]])

  /** @param consumes
    *   `@Consumes`, `None` when absent (JSON for typed bodies, text/plain for `String`, octet-stream for bytes).
    * @param params
    *   HTTP parameters in declaration order; [[ArgSlot.Param]] indexes into this list.
    * @param clauses
    *   all term parameter clauses, used to apply the method call.
    * @param isEffect
    *   `true` when the method returns `F[A]`, `false` for a plain `A`.
    * @param output
    *   the success type `A`.
    */
  final case class MethodSpec(
    symbol: Symbol,
    params: List[Param],
    body: Option[TypeRepr],
    clauses: List[Clause],
    consumes: Option[String],
    produces: Option[String],
    isEffect: Boolean,
    output: TypeRepr,
  )

  /** Human-readable form of `F[A]` for error messages; backends may override it. */
  def effectDescription(effect: TypeRepr): String = s"${effect.show}[A]"

  /** Types are taken as seen from `controller`, so a tagless-final `class Api[F[_]]` used as `Api[IO]` yields `IO[A]`
    * results.
    */
  def parseMethod(controller: TypeRepr, effect: TypeRepr, method: Symbol): MethodSpec = {
    val (clauseTypes, result) = signature(controller, method)
    val clauses = termClauses(method).zip(clauseTypes)
    val explicit = clauses.collect { case (syms, (types, false)) => syms.zip(types) }.flatten
    val (bodyParams, httpParams) = explicit.partition((sym, _) => sym.isAnnotatedWith(OkapiAnnotation.RequestBody))
    bodyParams.foreach((p, _) => checkBodyParam(p))
    bodyParams.drop(1).foreach((p, _) => error(p, s"Method ${method.name} can declare only one @RequestBody"))

    val httpIndex = httpParams.map(_._1).zipWithIndex.toMap
    val positions = clauses.flatMap(_._1).zipWithIndex.toMap // default getters are numbered across all clauses
    val (isEffect, output) = unwrapEffect(method, result, effect)
    MethodSpec(
      symbol = method,
      params = httpParams.map((sym, tpe) => toParam(controller, method, sym, tpe, positions(sym) + 1)),
      body = bodyParams.headOption.map(_._2),
      clauses = clauses.map {
        case (syms, (types, isImplicit)) =>
          if isImplicit then Clause(Nil, Some(types.map(summonArgument(method, _))))
          else Clause(syms.map(s => httpIndex.get(s).fold(ArgSlot.Body)(ArgSlot.Param(_))), None)
      },
      consumes = mediaType(method, OkapiAnnotation.Consumes),
      produces = mediaType(method, OkapiAnnotation.Produces),
      isEffect = isEffect,
      output = output,
    )
  }

  /** The declared, validated media type; `None` when not declared (each body kind then has its own default). */
  private def mediaType(method: Symbol, annotation: OkapiAnnotation): Option[String] = {
    method.nonEmptyAnnotationArg(annotation).map { m =>
      sttp.model.MediaType.parse(m).fold(e => abort(s"${annotation.show}(\"$m\") on '${method.name}': $e"), _ => m)
    }
  }

  /** Names of the method's `@Path` parameters. */
  def pathParamNames(method: Symbol): List[String] = {
    termClauses(method).flatten.collect {
      case p if paramKinds(p) == List(ParamKind.Path) => p.nonEmptyAnnotationArg(OkapiAnnotation.Path).getOrElse(p.name)
    }
  }

  /** The method's final result type, as seen from `controller`. */
  def resultType(controller: TypeRepr, method: Symbol): TypeRepr = signature(controller, method)._2

  /** Parameter types per term clause (with an "is implicit" flag) and the final result type. */
  private def signature(controller: TypeRepr, method: Symbol): (List[(List[TypeRepr], Boolean)], TypeRepr) = {
    def loop(tpe: TypeRepr): (List[(List[TypeRepr], Boolean)], TypeRepr) = {
      tpe match {
        case mt @ MethodType(_, params, result) =>
          val (rest, finalResult) = loop(result)
          ((params, mt.isImplicit) :: rest, finalResult)
        case ByNameType(result) => (Nil, result)
        case _: PolyType => abort(s"Controller method '${method.name}' must not declare type parameters")
        case result => (Nil, result)
      }
    }
    loop(controller.memberType(method))
  }

  private def termClauses(method: Symbol): List[List[Symbol]] =
    method.paramSymss.filter(_.headOption.forall(_.isTerm))

  private def toParam(controller: TypeRepr, method: Symbol, param: Symbol, tpe: TypeRepr, position: Int): Param = {
    val kind = paramKinds(param) match {
      case Nil =>
        warning(param, s"Parameter ${param.name} in ${method.name} has no HTTP annotation, defaulting to @Query")
        ParamKind.Query
      case single :: Nil => single
      case many =>
        error(
          param,
          s"Parameter ${param.name} has conflicting annotations: ${many.map(_.annotation.show).mkString(", ")}; " +
            "keep one",
        )
        many.head
    }
    val name = param.nonEmptyAnnotationArg(kind.annotation).getOrElse(param.name)
    Param(name, tpe, kind, defaultGetter(controller, method, param, kind, position))
  }

  private def defaultGetter(controller: TypeRepr, method: Symbol, param: Symbol, kind: ParamKind, position: Int)
    : Option[Symbol] = {
    val hasDefault = param.nearest(p => Option.when(p.flags.is(Flags.HasDefault))(())).isDefined
    if !hasDefault then None
    else if kind == ParamKind.Path || kind == ParamKind.BearerAuth then {
      warning(param, s"The default value of ${kind.annotation.show} parameter ${param.name} is ignored: it is required")
      None
    }
    else controller.typeSymbol.methodMember(s"${method.name}$$default$$$position").headOption
  }

  private def checkBodyParam(param: Symbol): Unit = {
    paramKinds(param).foreach { kind =>
      error(param, s"Parameter ${param.name} cannot be both @RequestBody and ${kind.annotation.show}")
    }
  }

  /** HTTP-parameter annotations of the nearest symbol in the parameter's lineage that has any. */
  def paramKinds(param: Symbol): List[ParamKind] = {
    param
      .nearest(p => Some(p.okapiAnnotations.flatMap((kind, _) => ParamKind.fromAnnotation(kind))).filter(_.nonEmpty))
      .getOrElse(Nil)
  }

  private def summonArgument(method: Symbol, tpe: TypeRepr): Term = {
    Implicits.search(tpe) match {
      case success: ImplicitSearchSuccess => success.tree
      case failure: ImplicitSearchFailure =>
        abort(s"No given ${tpe.show} for the implicit parameter of '${method.name}': ${failure.explanation}")
    }
  }

  /** `F[A]` → `(true, A)`, any other `A` → `(false, A)`. A result built from `F`'s type constructor that does not
    * conform to `F[A]` (e.g. with another error type) is a compile error.
    */
  private def unwrapEffect(method: Symbol, tpe: TypeRepr, effect: TypeRepr): (Boolean, TypeRepr) = {
    tpe.dealias.simplified match {
      case applied @ AppliedType(_, args) if args.nonEmpty && applied <:< effect.appliedTo(args.last) =>
        (true, args.last)
      case AppliedType(head, _) if head.typeSymbol == effectHead(effect) =>
        abort(
          s"Controller method '${method.name}' must return ${effectDescription(effect)} or a plain A. Got: ${tpe.show}"
        )
      case _ => (false, tpe)
    }
  }

  /** The class behind `F`: `M` for `[x] =>> M[R, E, x]` and for `M`. */
  private def effectHead(effect: TypeRepr): Symbol = {
    effect.dealias match {
      case TypeLambda(_, _, body) => body.dealias.typeSymbol
      case other => other.typeSymbol
    }
  }

  private def error(sym: Symbol, msg: String): Unit = sym.pos.fold(report.error(msg))(report.error(msg, _))

  private def warning(sym: Symbol, msg: String): Unit = sym.pos.fold(report.warning(msg))(report.warning(msg, _))
}
