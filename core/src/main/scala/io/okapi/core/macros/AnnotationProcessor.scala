package io.okapi.core.macros

import zio.*
import zio.http.{ Response, Routes }

import io.okapi.core.OkapiRuntime
import io.okapi.core.http.ApiError
import io.okapi.core.http.ApiError.ApiErrorResponse
import io.okapi.core.http.FileResponse
import scala.quoted.*
import sttp.tapir.*
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import sttp.tapir.ztapir.*

private[okapi] object AnnotationProcessor {

  // ── Constants ──────────────────────────────────────────────────────────────

  private val HttpAnnotations = Set("Get", "Post", "Put", "Delete", "Patch")
  private val WebSocketAnnotation = "WebSocket"

  // ── Public API ─────────────────────────────────────────────────────────────

  inline def endpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    ${ endpointsImpl[T] }

  inline def swaggerRoutes[T](title: String, version: String): Routes[Any, Response] =
    ${ swaggerRoutesImpl[T]('title, 'version) }

  inline def httpRoutes[T]: Routes[T, Response] =
    ${ httpRoutesImpl[T] }

  transparent inline def controllerLayers[Types <: Tuple] =
    ${ controllerLayersImpl[Types] }

  inline def autoLayer[Types <: Tuple]: Any =
    ${ autoLayerImpl[Types] }

  // ── Macro implementations ──────────────────────────────────────────────────

  private def swaggerRoutesImpl[T: Type](
    title: Expr[String],
    version: Expr[String],
  )(using Quotes
  ): Expr[Routes[Any, Response]] = {
    '{
      val publicEndpoints = AnnotationProcessor.endpoints[T].map(_.endpoint)
      val swaggerEndpoints = SwaggerInterpreter().fromEndpoints[Task](publicEndpoints, $title, $version)
      ZioHttpInterpreter().toHttp(swaggerEndpoints)
    }
  }

  private def httpRoutesImpl[T: Type](using Quotes): Expr[Routes[T, Response]] = {
    '{
      ZioHttpInterpreter()
        .asInstanceOf[sttp.tapir.server.ziohttp.ZioHttpInterpreter[T]]
        .toHttp(
          AnnotationProcessor
            .endpoints[T]
            .asInstanceOf[List[ZServerEndpoint[T, sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets]]]
        )
    }
  }

  private def endpointsImpl[T: Type](using Quotes): Expr[List[ZServerEndpoint[T, sttp.capabilities.WebSockets]]] = {
    import quotes.reflect.*

    val controllerTpe = TypeRepr.of[T]
    val controllerSym = controllerTpe.typeSymbol
    val basePath = annotationValue(controllerSym, "Controller").getOrElse("")
    val tag = annotationValue(controllerSym, "ApiTag")
      .filter(_.nonEmpty)
      .orElse(annotationValue(controllerSym, "Tag").filter(_.nonEmpty))
      .getOrElse(controllerSym.name)

    val restExprs = controllerSym
      .methodMembers
      .filter(m => m.annotations.exists(a => HttpAnnotations.contains(a.tpe.typeSymbol.name)))
      .sortBy { m =>
        // route precedence: fixed segments before captures at each position (lexicographic over a 0/1 mask),
        // so /books/stats is tried before /books/{id}, and /a/{x} before /{y}/b
        val ann = m.annotations.find(a => HttpAnnotations.contains(a.tpe.typeSymbol.name)).get
        pathSpecificityKey(normalizePath(basePath, extractStringArg(ann).getOrElse("")))
      }
      .map { m =>
        val ann = m.annotations.find(a => HttpAnnotations.contains(a.tpe.typeSymbol.name)).get
        val httpMethod = ann.tpe.typeSymbol.name
        val methodPath = extractStringArg(ann).getOrElse("")
        buildEndpoint[T](
          controllerTpe = controllerTpe,
          methodSym = m,
          httpMethod = httpMethod,
          fullPath = normalizePath(basePath, methodPath),
          tag = tag,
          summary = annotationValue(m, "Summary"),
          description = annotationValue(m, "Description"),
          deprecated = hasAnnotation(m, "Deprecated"),
        )
      }

    val wsExprs = controllerSym
      .methodMembers
      .filter(m => m.annotations.exists(a => a.tpe.typeSymbol.name == WebSocketAnnotation))
      .map { m =>
        val ann = m.annotations.find(a => a.tpe.typeSymbol.name == WebSocketAnnotation).get
        val methodPath = extractStringArg(ann).getOrElse("")
        buildWebSocketEndpoint[T](
          controllerTpe = controllerTpe,
          methodSym = m,
          fullPath = normalizePath(basePath, methodPath),
          tag = tag,
          summary = annotationValue(m, "Summary"),
          description = annotationValue(m, "Description"),
          deprecated = hasAnnotation(m, "Deprecated"),
        )
      }

    Expr.ofList(restExprs ++ wsExprs)
  }

  // ── Layer derivation ───────────────────────────────────────────────────────

  private def controllerLayersImpl[Types: Type](using q: Quotes): Expr[Any] = {
    import q.reflect.*

    val types = deconstructTuple(TypeRepr.of[Types])

    def deriveLayer(t: TypeRepr): Expr[ZLayer[?, Any, ?]] =
      t.asType match { case '[x] => '{ ZLayer.derive[x] }.asExprOf[ZLayer[?, Any, ?]] }

    types match {
      case Nil => '{ ZLayer.empty }
      case ts =>
        ts.reduceLeft[TypeRepr](AndType.apply).asType match {
          case '[output] =>
            '{ ZLayer.make[output].apply[Any](${ Varargs(ts.map(deriveLayer)) }*) }
        }
    }
  }

  private def autoLayerImpl[Types: Type](using q: Quotes): Expr[Any] = {
    import q.reflect.*

    val controllerTypes = deconstructTuple(TypeRepr.of[Types])
    if (controllerTypes.isEmpty) return '{ ZLayer.empty }

    def isBuiltIn(sym: Symbol): Boolean = {
      !sym.isClassDef || {
        val n = sym.fullName
        n.startsWith("scala.") || n.startsWith("java.") || n.startsWith("zio.") || n.startsWith("sttp.")
      }
    }

    val visited = scala.collection.mutable.LinkedHashSet.empty[Symbol]
    val allDeps = scala.collection.mutable.ListBuffer.empty[TypeRepr]

    def discoverDeps(tpe: TypeRepr): Unit = {
      val sym = tpe.dealias.typeSymbol
      if (sym == Symbol.noSymbol || visited.contains(sym) || isBuiltIn(sym)) return
      visited += sym
      val ctor = sym.primaryConstructor
      if (ctor != Symbol.noSymbol) {
        ctor
          .paramSymss
          .filter(g => g.headOption.forall(p => !p.flags.is(Flags.Given) && !p.flags.is(Flags.Implicit)))
          .flatten
          .foreach { p =>
            val pt = p.tree match { case vd: ValDef => vd.tpt.tpe; case _ => p.termRef.widen }
            discoverDeps(pt)
          }
      }
      allDeps += tpe.dealias
    }

    controllerTypes.foreach(discoverDeps)

    def deriveLayer(t: TypeRepr): Expr[ZLayer[?, Any, ?]] =
      t.asType match { case '[x] => '{ ZLayer.derive[x] }.asExprOf[ZLayer[?, Any, ?]] }

    controllerTypes.reduceLeft[TypeRepr](AndType.apply).asType match {
      case '[output] =>
        '{ ZLayer.make[output].apply[Any](${ Varargs(allDeps.toList.map(deriveLayer)) }*) }
    }
  }

  // ── Endpoint builders ──────────────────────────────────────────────────────

  private def buildEndpoint[T: Type](
    using q: Quotes
  )(
    controllerTpe: q.reflect.TypeRepr,
    methodSym: q.reflect.Symbol,
    httpMethod: String,
    fullPath: String,
    tag: String,
    summary: Option[String],
    description: Option[String],
    deprecated: Boolean = false,
  ): Expr[ZServerEndpoint[T, sttp.capabilities.WebSockets]] = {
    import q.reflect.*

    val ParsedMethod(params, body, consumesMediaType, isZioReturn, outputType, producesMediaType) = parseMethod(
      methodSym
    )

    val (endpointAfterPath, tapirOrderedNames, consumedPathParams) =
      applyPathInputs(baseEndpointTerm(httpMethod), fullPath, params, methodSym)

    val endpointAfterInputs = params
      .filter(p => p.kind != ParamKind.Path || !consumedPathParams.contains(p.name))
      .foldLeft(endpointAfterPath) { (acc, p) => applyInput(acc, parameterInputExpr(p).asTerm) }

    val endpointAfterBody = body.fold(endpointAfterInputs) { bodyTpe =>
      applyInput(endpointAfterInputs, requestBodyExpr(bodyTpe.asInstanceOf[TypeRepr], consumesMediaType).asTerm)
    }

    val outputTpe = outputType.asInstanceOf[TypeRepr]
    val endpointWithBody = applyOutput(endpointAfterBody, responseBodyExpr(outputTpe, producesMediaType).asTerm)
    val successStatus = resolveSuccessStatus(methodSym, httpMethod, outputTpe)
    val endpointWithStatus = {
      if successStatus == 200 then endpointWithBody
      else applyOutput(endpointWithBody, fixedStatusOutputExpr(successStatus).asTerm)
    }
    val endpointWithOutputs = applyErrorOut(endpointWithStatus, errorOutputExpr.asTerm)

    val endpointWithMeta = applyMetadata(endpointWithOutputs, tag, summary, description, deprecated)

    val logicTerm = buildServerLogic[T](
      controllerTpe = controllerTpe,
      methodSym = methodSym,
      params = params,
      body = body,
      isZioReturn = isZioReturn,
      outputType = outputType,
      tapirParamOrder = tapirOrderedNames,
    )

    attachServerLogic[T](endpointWithMeta, logicTerm)
  }

  private def buildWebSocketEndpoint[T: Type](
    using q: Quotes
  )(
    controllerTpe: q.reflect.TypeRepr,
    methodSym: q.reflect.Symbol,
    fullPath: String,
    tag: String,
    summary: Option[String],
    description: Option[String],
    deprecated: Boolean = false,
  ): Expr[ZServerEndpoint[T, sttp.capabilities.WebSockets]] = {
    import q.reflect.*

    val ParsedMethod(params, _, _, isZioReturn, outputType, _) = parseMethod(methodSym)
    val wsTpe = outputType.asInstanceOf[TypeRepr]

    val (inType, outType) = extractWsPipeArgs(wsTpe).getOrElse {
      report.errorAndAbort(
        s"@WebSocket method '${methodSym.name}' must return WsPipe[In, Out] or IO[ApiError, WsPipe[In, Out]]. " +
          s"Got: ${wsTpe.show}"
      )
    }

    val (endpointAfterPath, tapirOrderedNames, consumedPathParams) =
      applyPathInputs('{ sttp.tapir.endpoint.get }.asTerm, fullPath, params, methodSym)

    val endpointAfterParams = params
      .filter(p => p.kind != ParamKind.Path || !consumedPathParams.contains(p.name))
      .foldLeft(endpointAfterPath) { (acc, p) => applyInput(acc, parameterInputExpr(p).asTerm) }

    val wsHelperName = {
      if inType =:= TypeRepr.of[String] && outType =:= TypeRepr.of[String] then "addTextWsOutput"
      else if inType <:< TypeRepr.of[Array[Byte]] && outType <:< TypeRepr.of[Array[Byte]] then "addBinaryWsOutput"
      else {
        report.errorAndAbort(
          s"@WebSocket '${methodSym.name}': unsupported message types In=${inType.show} Out=${outType.show}. " +
            "Supported: WsPipe[String, String] (text) or WsPipe[Array[Byte], Array[Byte]] (binary)."
        )
      }
    }

    val (wsSi, wsI, wsE, _, _) = endpointBaseTypeParts(endpointAfterParams)
    val endpointWithWs = Apply(
      TypeApply(runtimeSelect(wsHelperName), List(Inferred(wsSi), Inferred(wsI), Inferred(wsE))),
      List(endpointAfterParams),
    )

    val endpointWithMeta = applyMetadata(
      applyErrorOut(endpointWithWs, errorOutputExpr.asTerm),
      tag,
      summary,
      description,
      deprecated,
    )

    val logicTerm = buildServerLogic[T](
      controllerTpe = controllerTpe,
      methodSym = methodSym,
      params = params,
      body = None,
      isZioReturn = isZioReturn,
      outputType = wsTpe,
      tapirParamOrder = tapirOrderedNames,
    )

    val (_, wsInputType, wsErrorType, _, _) = endpointBaseTypeParts(endpointWithMeta)
    Apply(
      TypeApply(
        runtimeSelect("attachWsServerLogic"),
        List(
          Inferred(TypeRepr.of[T]),
          Inferred(wsInputType),
          Inferred(wsErrorType),
          Inferred(inType),
          Inferred(outType),
        ),
      ),
      List(endpointWithMeta, logicTerm),
    ).asExprOf[ZServerEndpoint[T, sttp.capabilities.WebSockets]]
  }

  // ── Success status code ────────────────────────────────────────────────────

  /** Explicit `@Status(code)` wins; otherwise 204 for a `Unit` body, 201 for POST, 200 for everything else. */
  private def resolveSuccessStatus(
    using q: Quotes
  )(
    methodSym: q.reflect.Symbol,
    httpMethod: String,
    outputTpe: q.reflect.TypeRepr,
  ): Int = {
    import q.reflect.*
    statusAnnotationValue(methodSym).getOrElse {
      if outputTpe =:= TypeRepr.of[Unit] then 204
      else if httpMethod == "Post" then 201
      else 200
    }
  }

  private def statusAnnotationValue(using q: Quotes)(sym: q.reflect.Symbol): Option[Int] =
    sym.annotations.collectFirst { case ann if ann.tpe.typeSymbol.name == "Status" => extractIntArg(ann) }.flatten

  private def fixedStatusOutputExpr(using Quotes)(code: Int): Expr[EndpointOutput[Unit]] =
    '{ sttp.tapir.statusCode(sttp.model.StatusCode(${ Expr(code) })) }

  // ── Shared endpoint helpers ────────────────────────────────────────────────

  /** Walks the URL template, appending fixed-path segments and path-param inputs in URL order. Returns the updated
    * endpoint term, the Tapir-ordered param names, and the set of path param names that were consumed from the URL
    * template.
    *
    * Tapir tuple order: consumed path params (URL order), then non-path params (declaration order), with the body
    * always last. Path params declared but absent from the URL template are NOT consumed here — they must be applied
    * after the URL walk (see buildEndpoint).
    */
  private def applyPathInputs(
    using q: Quotes
  )(
    initial: q.reflect.Term,
    fullPath: String,
    params: List[ParamInfo],
    methodSym: q.reflect.Symbol,
  ): (q.reflect.Term, List[String], Set[String]) = {
    import q.reflect.*

    val pathParamsByName = params.filter(_.kind == ParamKind.Path).map(p => p.name -> p).toMap
    val consumedInOrder = scala.collection.mutable.ListBuffer.empty[String]
    var current = initial

    fullPath.split('/').nn.toList.filter(_.nonEmpty).foreach { segment =>
      if (segment.startsWith("{") && segment.endsWith("}") && segment.length > 2) {
        val paramName = segment.substring(1, segment.length - 1).nn
        pathParamsByName.get(paramName) match {
          case Some(p) =>
            consumedInOrder += paramName
            current = applyInput(current, parameterInputExpr(p).asTerm)
          case None =>
            report.errorAndAbort(
              s"Path template '$fullPath' contains {$paramName} but no @Path(\"$paramName\") parameter " +
                s"in method '${methodSym.name}'"
            )
        }
      }
      else {
        val fixedSegment = '{
          sttp
            .tapir
            .EndpointInput
            .FixedPath(
              ${ Expr(segment) },
              sttp.tapir.Codec.idPlain(),
              sttp.tapir.EndpointIO.Info.empty,
            )
        }.asTerm
        current = applyInput(current, fixedSegment)
      }
    }

    val consumed = consumedInOrder.toSet
    // Tapir tuple order must mirror the order inputs are actually applied (see buildEndpoint /
    // buildWebSocketEndpoint): consumed path params (URL order) first, then every remaining param
    // (unconsumed @Path + non-path) in DECLARATION order — NOT grouped by kind, otherwise the
    // indices computed in extractArgs are wrong whenever the two interleave.
    val remainingInDeclOrder = params.filter(p => p.kind != ParamKind.Path || !consumed.contains(p.name)).map(_.name)
    val tapirOrderedNames = consumedInOrder.toList ++ remainingInDeclOrder
    (current, tapirOrderedNames, consumed)
  }

  /** Applies tag, summary, description, and deprecated to an endpoint term. */
  private def applyMetadata(
    using q: Quotes
  )(
    endpointTerm: q.reflect.Term,
    tag: String,
    summary: Option[String],
    description: Option[String],
    deprecated: Boolean,
  ): q.reflect.Term = {
    import q.reflect.*
    var current = applyScalarOp(endpointTerm, "tag", Expr(tag).asTerm)
    summary.foreach { v => current = applyScalarOp(current, "summary", Expr(v).asTerm) }
    description.foreach { v => current = applyScalarOp(current, "description", Expr(v).asTerm) }
    if (deprecated) current = applyEndpointNoArgMethod(current, "deprecated")
    current
  }

  private def attachServerLogic[T: Type](
    using q: Quotes
  )(
    endpointTerm: q.reflect.Term,
    logicTerm: q.reflect.Term,
  ): Expr[ZServerEndpoint[T, sttp.capabilities.WebSockets]] = {
    import q.reflect.*
    val (_, inputType, errorType, outputType, _) = endpointBaseTypeParts(endpointTerm)
    Apply(
      TypeApply(
        runtimeSelect("attachServerLogic"),
        List(Inferred(TypeRepr.of[T]), Inferred(inputType), Inferred(errorType), Inferred(outputType)),
      ),
      List(endpointTerm, logicTerm),
    ).asExprOf[ZServerEndpoint[T, sttp.capabilities.WebSockets]]
  }

  // ── Server logic ───────────────────────────────────────────────────────────

  private def buildServerLogic[T: Type](
    using q: Quotes
  )(
    controllerTpe: q.reflect.TypeRepr,
    methodSym: q.reflect.Symbol,
    params: List[ParamInfo],
    body: Option[Any],
    isZioReturn: Boolean,
    outputType: Any,
    tapirParamOrder: List[String] = Nil,
  ): q.reflect.Term = {
    import q.reflect.*

    // the server-logic lambda receives Tapir's input tuple, which is in TAPIR order (consumed path params in URL
    // order, then the remaining params in declaration order) — not declaration order. Reorder the param types to
    // match the endpoint's actual input tuple; extractArgs then re-indexes back to declaration order by name.
    val orderedParams = {
      if tapirParamOrder.nonEmpty then tapirParamOrder.flatMap(n => params.find(_.name == n))
      else params
    }
    val userInputTypes = orderedParams.map(_.tpe.asInstanceOf[TypeRepr]) ++ body.toList.map(_.asInstanceOf[TypeRepr])
    val inputType = tupleType(userInputTypes)
    val controllerOutputType = outputType.asInstanceOf[TypeRepr]
    val isFileResponse = controllerOutputType =:= TypeRepr.of[FileResponse]
    val successType = if isFileResponse then TypeRepr.of[(Array[Byte], String)] else controllerOutputType
    val mappedErrorType = TypeRepr.of[(sttp.model.StatusCode, ApiErrorResponse)]

    val zioTc = TypeRepr.of[ZIO[Any, Any, Any]] match {
      case AppliedType(tc, _) => tc
      case other => report.errorAndAbort(s"Unexpected ZIO type shape: ${other.show}")
    }
    val outerReturnType = AppliedType(zioTc, List(controllerTpe, mappedErrorType, successType))
    val innerReturnType = AppliedType(zioTc, List(TypeRepr.of[Any], mappedErrorType, successType))

    Lambda(
      Symbol.spliceOwner,
      MethodType(List("input"))(_ => List(inputType), _ => outerReturnType),
      (owner, paramsTerms) => {
        val inputTerm = paramsTerms.head.asInstanceOf[Term]

        val controllerLambda = Lambda(
          owner,
          MethodType(List("controller"))(_ => List(controllerTpe), _ => innerReturnType),
          (_, controllerTerms) => {
            val controllerTerm = controllerTerms.head.asInstanceOf[Term]
            val args = extractArgs(inputTerm, params, body, tapirParamOrder)
            val methodCall = callControllerMethod(methodSym, args, controllerTerm)
            val zioCall = if isZioReturn then methodCall else wrapInZioSucceed(methodCall, controllerOutputType)
            if isFileResponse then mapFileResponseError(zioCall) else mapApiError(zioCall, successType)
          },
        )

        Apply(
          TypeApply(runtimeSelect("serviceWithMappedZio"), List(Inferred(controllerTpe), Inferred(successType))),
          List(controllerLambda),
        )
      },
    )
  }

  private def extractArgs(
    using q: Quotes
  )(
    inputTerm: q.reflect.Term,
    params: List[ParamInfo],
    body: Option[Any],
    tapirParamOrder: List[String] = Nil,
  ): List[q.reflect.Term] = {
    import q.reflect.*

    val allTypes = params.map(_.tpe.asInstanceOf[TypeRepr]) ++ body.toList.map(_.asInstanceOf[TypeRepr])

    if (allTypes.isEmpty) Nil
    else if (allTypes.length == 1) List(castTerm(inputTerm, allTypes.head))
    else {
      val asProduct = TypeApply(Select.unique(inputTerm, "asInstanceOf"), List(Inferred(TypeRepr.of[Product])))

      def element(idx: Int, tpe: TypeRepr): Term =
        castTerm(Apply(Select.unique(asProduct, "productElement"), List(Literal(IntConstant(idx)))), tpe)

      val paramArgs = params.map { p =>
        val idx = if tapirParamOrder.nonEmpty then tapirParamOrder.indexOf(p.name) else params.indexOf(p)
        element(idx, p.tpe.asInstanceOf[TypeRepr])
      }
      val bodyIdx = if tapirParamOrder.nonEmpty then tapirParamOrder.length else params.length
      val bodyArgs = body.toList.map(tpe => element(bodyIdx, tpe.asInstanceOf[TypeRepr]))
      paramArgs ++ bodyArgs
    }
  }

  private def callControllerMethod(
    using q: Quotes
  )(
    methodSym: q.reflect.Symbol,
    args: List[q.reflect.Term],
    controllerTerm: q.reflect.Term,
  ): q.reflect.Term = {
    import q.reflect.*
    val select = Select.unique(controllerTerm, methodSym.name)
    if (args.isEmpty && acceptsEmptyArgumentList(methodSym)) Apply(select, Nil)
    else if (args.isEmpty) select
    else Apply(select, args)
  }

  private def acceptsEmptyArgumentList(using q: Quotes)(methodSym: q.reflect.Symbol): Boolean = {
    methodSym.tree match {
      case defDef: q.reflect.DefDef =>
        defDef.termParamss.nonEmpty && defDef.termParamss.forall(_.params.isEmpty)
      case _ => false
    }
  }

  private def wrapInZioSucceed(using q: Quotes)(term: q.reflect.Term, outputType: q.reflect.TypeRepr)
    : q.reflect.Term = {
    import q.reflect.*
    Apply(TypeApply(runtimeSelect("liftPure"), List(Inferred(outputType))), List(term))
  }

  private def mapFileResponseError(using q: Quotes)(zioCall: q.reflect.Term): q.reflect.Term = {
    import q.reflect.*
    Apply(TypeApply(runtimeSelect("mapFileResponseApiError"), List(Inferred(TypeRepr.of[Any]))), List(zioCall))
  }

  private def mapApiError(using q: Quotes)(zioCall: q.reflect.Term, successType: q.reflect.TypeRepr): q.reflect.Term = {
    import q.reflect.*
    Apply(
      TypeApply(runtimeSelect("mapApiErrorEffect"), List(Inferred(TypeRepr.of[Any]), Inferred(successType))),
      List(zioCall),
    )
  }

  // ── Endpoint operations ────────────────────────────────────────────────────

  private enum TransputSlot { case Input, ErrorOut, Output }

  /** Shared logic behind addInput / addErrorOutput / addOutput: pulls the endpoint's 5 type parameters, widens the
    * accumulator named by `slot`, and emits the matching OkapiRuntime call with the ParamConcat evidence. The 7 type
    * arguments are identical across the three operations — only which accumulator feeds ParamConcat and the runtime
    * method name differ.
    */
  private def applyTransput(
    using q: Quotes
  )(
    endpoint: q.reflect.Term,
    transput: q.reflect.Term,
    slot: TransputSlot,
  ): q.reflect.Term = {
    import q.reflect.*
    val (si, i, e, o, c) = endpointTypeParts(endpoint)
    val (cur, runtimeName) = slot match {
      case TransputSlot.Input => (i, "addInput")
      case TransputSlot.ErrorOut => (e, "addErrorOutput")
      case TransputSlot.Output => (o, "addOutput")
    }
    val next = transputValueType(transput.tpe)
    val combined = concatValueTypes(cur, next)
    val concat = summonParamConcat(cur, next)
    Apply(
      Apply(
        TypeApply(
          runtimeSelect(runtimeName),
          List(Inferred(si), Inferred(i), Inferred(e), Inferred(o), Inferred(c), Inferred(next), Inferred(combined)),
        ),
        List(endpoint, transput),
      ),
      List(concat),
    )
  }

  private def applyInput(using q: Quotes)(endpoint: q.reflect.Term, input: q.reflect.Term): q.reflect.Term =
    applyTransput(endpoint, input, TransputSlot.Input)

  private def applyOutput(using q: Quotes)(endpoint: q.reflect.Term, output: q.reflect.Term): q.reflect.Term =
    applyTransput(endpoint, output, TransputSlot.Output)

  private def applyErrorOut(using q: Quotes)(endpoint: q.reflect.Term, errOut: q.reflect.Term): q.reflect.Term =
    applyTransput(endpoint, errOut, TransputSlot.ErrorOut)

  private def applyScalarOp(
    using q: Quotes
  )(
    endpoint: q.reflect.Term,
    methodName: String,
    argument: q.reflect.Term,
  ): q.reflect.Term = {
    import q.reflect.*
    val sym = endpoint
      .tpe
      .widenTermRefByName
      .typeSymbol
      .methodMember(methodName)
      .find(m => m.paramSymss.find(_.headOption.exists(_.isTerm)).exists(_.size == 1))
      .getOrElse(report.errorAndAbort(s"Cannot resolve method '$methodName' on ${endpoint.tpe.show}"))
    Apply(Select(endpoint, sym), List(argument))
  }

  private def applyEndpointNoArgMethod(using q: Quotes)(endpoint: q.reflect.Term, methodName: String)
    : q.reflect.Term = {
    import q.reflect.*
    val sym = endpoint
      .tpe
      .widenTermRefByName
      .typeSymbol
      .methodMember(methodName)
      .headOption
      .getOrElse(report.errorAndAbort(s"Cannot resolve zero-arg method '$methodName' on ${endpoint.tpe.show}"))
    Apply(Select(endpoint, sym), Nil)
  }

  // ── Parameter expressions ──────────────────────────────────────────────────

  private def summonCodecOrAbort[C: Type](missing: => String)(using q: Quotes): Expr[C] =
    Expr.summon[C].getOrElse(q.reflect.report.errorAndAbort(missing))

  private def parameterInputExpr(using q: Quotes)(param: ParamInfo): Expr[EndpointInput[?]] = {
    val tpe = param.tpe.asInstanceOf[q.reflect.TypeRepr]
    def missing(codec: String): String = {
      s"Missing $codec for @${param.kind.annotationName}(\"${param.name}\") of type ${tpe.show}. " +
        "Provide a Tapir Codec (e.g. import sttp.tapir.generic.auto.*)."
    }
    tpe.asType match {
      case '[t] =>
        val name = Expr(param.name)
        param.kind match {
          case ParamKind.Path =>
            val codec =
              summonCodecOrAbort[Codec[String, t, CodecFormat.TextPlain]](missing("Codec[String, T, TextPlain]"))
            '{ EndpointInput.PathCapture(Some($name), $codec, EndpointIO.Info.empty) }
          case ParamKind.Query =>
            val codec = {
              summonCodecOrAbort[Codec[List[String], t, CodecFormat.TextPlain]](
                missing("Codec[List[String], T, TextPlain]")
              )
            }
            '{ EndpointInput.Query($name, None, $codec, EndpointIO.Info.empty) }
          case ParamKind.Header =>
            val codec = {
              summonCodecOrAbort[Codec[List[String], t, CodecFormat.TextPlain]](
                missing("Codec[List[String], T, TextPlain]")
              )
            }
            '{ EndpointIO.Header($name, $codec, EndpointIO.Info.empty) }
          case ParamKind.Cookie =>
            val codec = {
              summonCodecOrAbort[Codec[Option[String], t, CodecFormat.TextPlain]](
                missing("Codec[Option[String], T, TextPlain]")
              )
            }
            '{ EndpointInput.Cookie($name, $codec, EndpointIO.Info.empty) }
          case ParamKind.BearerAuth =>
            val codec = {
              summonCodecOrAbort[Codec[List[String], t, CodecFormat.TextPlain]](
                missing("Codec[List[String], T, TextPlain]")
              )
            }
            '{ sttp.tapir.TapirAuth.bearer[t]()(using $codec) }
        }
    }
  }

  private def requestBodyExpr(using q: Quotes)(tpe: q.reflect.TypeRepr, mediaType: String): Expr[EndpointInput[?]] = {
    import q.reflect.*
    if tpe <:< TypeRepr.of[Array[Byte]] then '{ sttp.tapir.byteArrayBody }
    else if tpe <:< TypeRepr.of[String] then stringInputForMediaType(mediaType)
    else if isZStreamOfByte(tpe) then
      '{
        EndpointIO.StreamBodyWrapper(
          sttp
            .tapir
            .streamBody(sttp.capabilities.zio.ZioStreams)(
              sttp.tapir.Schema.binary,
              sttp.tapir.CodecFormat.OctetStream(),
            )
        )
      }
    else {
      mediaType match {
        case "application/x-www-form-urlencoded" => formBodyExpr(tpe)
        case "multipart/form-data" => multipartBodyExpr(tpe)
        case _ => jsonBodyExpr(tpe)
      }
    }
  }

  private def stringInputForMediaType(using Quotes)(mediaType: String): Expr[EndpointInput[?]] = {
    mediaType match {
      case "text/html" => '{ sttp.tapir.htmlBodyUtf8 }
      case "text/xml" | "application/xml" => '{ OkapiRuntime.xmlStringBody }
      case "text/javascript" | "application/javascript" => '{ OkapiRuntime.jsStringBody }
      case _ => '{ sttp.tapir.stringBody }
    }
  }

  private def formBodyExpr(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[EndpointInput[?]] = {
    tpe.asType match {
      case '[t] =>
        Expr
          .summon[Codec[String, t, CodecFormat.XWwwFormUrlencoded]]
          .map(codec => '{ OkapiRuntime.formBodyInput[t]($codec) })
          .getOrElse(
            q.reflect
              .report
              .errorAndAbort(
                s"Missing Codec[String, ${tpe.show}, CodecFormat.XWwwFormUrlencoded] for @Consumes(\"application/x-www-form-urlencoded\"). " +
                  "Import sttp.tapir.generic.auto.*"
              )
          )
    }
  }

  private def multipartBodyExpr(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[EndpointInput[?]] = {
    tpe.asType match {
      case '[t] =>
        Expr
          .summon[MultipartCodec[t]]
          .map(codec => '{ OkapiRuntime.multipartBodyInput[t]($codec) })
          .getOrElse(
            q.reflect
              .report
              .errorAndAbort(
                s"Missing MultipartCodec[${tpe.show}] for @Consumes(\"multipart/form-data\"). Import sttp.tapir.generic.auto.*"
              )
          )
    }
  }

  private def jsonBodyExpr(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[EndpointInput[?]] = {
    tpe.asType match {
      case '[t] =>
        (Expr.summon[zio.json.JsonCodec[t]], summonOrDeriveSchema[t]) match {
          case (Some(codec), Some(schema)) => '{ OkapiRuntime.jsonBodyInput[t]($codec, $schema) }
          case (None, _) => q.reflect.report.errorAndAbort(s"Missing zio.json.JsonCodec[${tpe.show}] for request body")
          case (_, None) => q.reflect.report.errorAndAbort(s"Missing sttp.tapir.Schema[${tpe.show}] for request body")
        }
    }
  }

  private def responseBodyExpr(using q: Quotes)(tpe: q.reflect.TypeRepr, mediaType: String): Expr[EndpointOutput[?]] = {
    import q.reflect.*
    if tpe =:= TypeRepr.of[Unit] then '{ sttp.tapir.emptyOutput }
    else if tpe =:= TypeRepr.of[FileResponse] then
      '{ sttp.tapir.byteArrayBody.and(sttp.tapir.header[String]("Content-Disposition")) }
    else if tpe <:< TypeRepr.of[Array[Byte]] then '{ sttp.tapir.byteArrayBody }
    else if isZStreamOfByte(tpe) then
      '{
        EndpointIO.StreamBodyWrapper(
          sttp
            .tapir
            .streamBody(sttp.capabilities.zio.ZioStreams)(
              sttp.tapir.Schema.binary,
              sttp.tapir.CodecFormat.OctetStream(),
            )
        )
      }
    else if tpe <:< TypeRepr.of[String] then stringOutputForMediaType(mediaType)
    else {
      mediaType match {
        case "text/html" => '{ sttp.tapir.htmlBodyUtf8 }
        case t if t.startsWith("text/") => '{ sttp.tapir.stringBody }
        case t if t.startsWith("image/") => '{ sttp.tapir.byteArrayBody }
        case "application/octet-stream" | "application/pdf" | "application/zip" => '{ sttp.tapir.byteArrayBody }
        case "application/x-www-form-urlencoded" =>
          tpe.asType match {
            case '[t] =>
              Expr
                .summon[Codec[String, t, CodecFormat.XWwwFormUrlencoded]]
                .map(codec => '{ OkapiRuntime.formBodyOutput[t]($codec) })
                .getOrElse(
                  report.errorAndAbort(
                    s"Missing Codec[String, ${tpe.show}, CodecFormat.XWwwFormUrlencoded] for @Produces(\"application/x-www-form-urlencoded\")"
                  )
                )
          }
        case _ => jsonOutputExpr(tpe)
      }
    }
  }

  private def stringOutputForMediaType(using Quotes)(mediaType: String): Expr[EndpointOutput[?]] = {
    mediaType match {
      case "text/html" => '{ sttp.tapir.htmlBodyUtf8 }
      case "text/xml" | "application/xml" => '{ OkapiRuntime.xmlStringBody }
      case "text/javascript" | "application/javascript" => '{ OkapiRuntime.jsStringBody }
      case "text/event-stream" => '{ OkapiRuntime.eventStreamBody }
      case _ => '{ sttp.tapir.stringBody }
    }
  }

  private def jsonOutputExpr(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[EndpointOutput[?]] = {
    if tpe =:= q.reflect.TypeRepr.of[Unit] then '{ sttp.tapir.emptyOutput }
    else if tpe <:< q.reflect.TypeRepr.of[String] then '{ sttp.tapir.stringBody }
    else {
      tpe.asType match {
        case '[t] =>
          (Expr.summon[zio.json.JsonCodec[t]], summonOrDeriveSchema[t]) match {
            case (Some(codec), Some(schema)) => '{ OkapiRuntime.jsonBodyOutput[t]($codec, $schema) }
            case (None, _) =>
              q.reflect.report.errorAndAbort(s"Missing zio.json.JsonCodec[${tpe.show}] for response body")
            case (_, None) =>
              q.reflect.report.errorAndAbort(s"Missing sttp.tapir.Schema[${tpe.show}] for response body")
          }
      }
    }
  }

  private def errorOutputExpr(using q: Quotes): Expr[EndpointOutput[(sttp.model.StatusCode, ApiErrorResponse)]] = {
    summonOrDeriveSchema[ApiErrorResponse]
      .map { schema =>
        '{
          sttp
            .tapir
            .statusCode
            .and(
              OkapiRuntime.jsonBodyOutput[ApiErrorResponse](summon[zio.json.JsonCodec[ApiErrorResponse]], $schema)
            )
        }
      }
      .getOrElse(q.reflect.report.errorAndAbort("Missing sttp.tapir.Schema[ApiErrorResponse] for error output"))
  }

  // ── Type utilities ─────────────────────────────────────────────────────────

  /** Summons Schema[T] directly, or derives it via Mirror when available. */
  private def summonOrDeriveSchema[T: Type](using Quotes): Option[Expr[Schema[T]]] = {
    Expr.summon[Schema[T]].orElse {
      Expr.summon[scala.deriving.Mirror.Of[T]].map { mirror =>
        '{ Schema.derived[T](using sttp.tapir.generic.Configuration.default, $mirror) }
      }
    }
  }

  /** Extracts the 5 type parameters from an endpoint term using a direct type match. Use this during incremental
    * endpoint building (applyInput / applyOutput / applyErrorOut).
    */
  private def endpointTypeParts(
    using q: Quotes
  )(
    term: q.reflect.Term
  ): (q.reflect.TypeRepr, q.reflect.TypeRepr, q.reflect.TypeRepr, q.reflect.TypeRepr, q.reflect.TypeRepr) = {
    import q.reflect.*
    term.tpe.widenTermRefByName match {
      case AppliedType(_, List(si, i, e, o, c)) => (si, i, e, o, c)
      case other => report.errorAndAbort(s"Unexpected endpoint type shape: ${other.show}")
    }
  }

  /** Like endpointTypeParts but resolves via baseType — needed after all operations are applied (e.g. before attaching
    * server logic) when the concrete type may be a subtype of Endpoint.
    */
  private def endpointBaseTypeParts(
    using q: Quotes
  )(
    term: q.reflect.Term
  ): (q.reflect.TypeRepr, q.reflect.TypeRepr, q.reflect.TypeRepr, q.reflect.TypeRepr, q.reflect.TypeRepr) = {
    import q.reflect.*
    term.tpe.widenTermRefByName.baseType(TypeRepr.of[Endpoint[Any, Any, Any, Any, Any]].typeSymbol) match {
      case AppliedType(_, List(si, i, e, o, c)) => (si, i, e, o, c)
      case other => report.errorAndAbort(s"Unexpected endpoint base type shape: ${other.show}")
    }
  }

  private def transputValueType(using q: Quotes)(tpe: q.reflect.TypeRepr): q.reflect.TypeRepr = {
    import q.reflect.*
    val w = tpe.widenTermRefByName
    w.baseType(TypeRepr.of[EndpointInput[Any]].typeSymbol) match {
      case AppliedType(_, List(v)) => v
      case _ =>
        w.baseType(TypeRepr.of[EndpointOutput[Any]].typeSymbol) match {
          case AppliedType(_, List(v)) => v
          case _ => report.errorAndAbort(s"Unexpected transput type: ${w.show}")
        }
    }
  }

  private def flattenTupleTypes(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = {
    import q.reflect.*
    if (tpe =:= TypeRepr.of[Unit] || tpe =:= TypeRepr.of[EmptyTuple]) Nil
    else {
      val tupleCons = TypeRepr.of[*:[Any, EmptyTuple]] match {
        case AppliedType(tc, _) => tc
        case other => report.errorAndAbort(s"Unexpected tuple cons shape: ${other.show}")
      }
      tpe.dealias match {
        case AppliedType(cons, List(head, tail)) if cons =:= tupleCons => head :: flattenTupleTypes(tail)
        case other => List(other)
      }
    }
  }

  private def concatValueTypes(using q: Quotes)(left: q.reflect.TypeRepr, right: q.reflect.TypeRepr)
    : q.reflect.TypeRepr =
    tupleType(flattenTupleTypes(left) ++ flattenTupleTypes(right))

  private def summonParamConcat(using q: Quotes)(left: q.reflect.TypeRepr, right: q.reflect.TypeRepr)
    : q.reflect.Term = {
    import q.reflect.*
    val paramConcatTpe = TypeRepr.of[sttp.tapir.typelevel.ParamConcat[Any, Any]] match {
      case AppliedType(tc, _) => AppliedType(tc, List(left, right))
      case other => report.errorAndAbort(s"Unexpected ParamConcat shape: ${other.show}")
    }
    Implicits.search(paramConcatTpe) match {
      case s: ImplicitSearchSuccess => s.tree
      case f: ImplicitSearchFailure =>
        report.errorAndAbort(s"Cannot summon ParamConcat[${left.show}, ${right.show}]: ${f.explanation}")
    }
  }

  private def tupleType(using q: Quotes)(types: List[q.reflect.TypeRepr]): q.reflect.TypeRepr = {
    import q.reflect.*
    types match {
      case Nil => TypeRepr.of[Unit]
      case only :: Nil => only
      case many =>
        val tupleCons = TypeRepr.of[*:[Any, EmptyTuple]] match {
          case AppliedType(tc, _) => tc
          case other => report.errorAndAbort(s"Unexpected tuple cons shape: ${other.show}")
        }
        many.foldRight(TypeRepr.of[EmptyTuple])((h, t) => AppliedType(tupleCons, List(h, t)))
    }
  }

  /** Deconstructs a Tuple type into its element TypeReprs. */
  private def deconstructTuple(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = {
    import q.reflect.*
    tpe.dealias match {
      case applied @ AppliedType(_, List(head, tail)) if applied.typeSymbol.fullName == "scala.*:" =>
        head :: deconstructTuple(tail)
      case AppliedType(_, args) if tpe.dealias.typeSymbol.fullName.startsWith("scala.Tuple") =>
        args
      case empty if empty =:= TypeRepr.of[EmptyTuple] =>
        Nil
      case other =>
        report.errorAndAbort(s"Expected a tuple of types, got: ${other.show}")
    }
  }

  // ── Term utilities ─────────────────────────────────────────────────────────

  private def castTerm(using q: Quotes)(term: q.reflect.Term, tpe: q.reflect.TypeRepr): q.reflect.Term = {
    import q.reflect.*
    TypeApply(Select.unique(term, "asInstanceOf"), List(Inferred(tpe)))
  }

  /** Selects a member of OkapiRuntime ready for TypeApply or Apply. */
  private def runtimeSelect(using q: Quotes)(name: String): q.reflect.Term = {
    import q.reflect.*
    Select.unique(Ref(Symbol.requiredModule("io.okapi.core.OkapiRuntime")), name)
  }

  private def baseEndpointTerm(using q: Quotes)(httpMethod: String): q.reflect.Term = {
    import q.reflect.*
    httpMethod match {
      case "Get" => '{ sttp.tapir.endpoint.get }.asTerm
      case "Post" => '{ sttp.tapir.endpoint.post }.asTerm
      case "Put" => '{ sttp.tapir.endpoint.put }.asTerm
      case "Delete" => '{ sttp.tapir.endpoint.delete }.asTerm
      case "Patch" => '{ sttp.tapir.endpoint.patch }.asTerm
      case other => report.errorAndAbort(s"Unsupported HTTP method annotation: $other")
    }
  }

  private def isZStreamOfByte(using q: Quotes)(tpe: q.reflect.TypeRepr): Boolean = {
    import q.reflect.*
    tpe.dealias match {
      case AppliedType(tc, List(_, _, elem)) if tc.typeSymbol.fullName == "zio.stream.ZStream" =>
        elem.dealias =:= TypeRepr.of[Byte]
      case _ => false
    }
  }

  private def extractWsPipeArgs(using q: Quotes)(tpe: q.reflect.TypeRepr)
    : Option[(q.reflect.TypeRepr, q.reflect.TypeRepr)] = {
    import q.reflect.*
    val fn1Sym = defn.FunctionClass(1)
    val zstreamSym = TypeRepr.of[zio.stream.ZStream[Any, Throwable, Any]].typeSymbol
    tpe.dealias match {
      case AppliedType(fn, List(inStream, outStream)) if fn.typeSymbol == fn1Sym =>
        (inStream.dealias, outStream.dealias) match {
          case (AppliedType(zsi, List(_, _, inType)), AppliedType(zso, List(_, _, outType)))
               if zsi.typeSymbol == zstreamSym && zso.typeSymbol == zstreamSym =>
            Some((inType, outType))
          case _ => None
        }
      case _ => None
    }
  }

  // ── Method parsing ─────────────────────────────────────────────────────────

  private def parseMethod(using q: Quotes)(methodSym: q.reflect.Symbol): ParsedMethod = {
    import q.reflect.*

    var bodyType: Option[TypeRepr] = None

    val params = methodSym.paramSymss.flatten.flatMap { param =>
      val tpe = param.tree match {
        case vd: ValDef => vd.tpt.tpe
        case _ => param.termRef.widen
      }

      if (hasAnnotation(param, "RequestBody")) {
        if (bodyType.nonEmpty) {
          val msg = s"Method ${methodSym.name} can declare only one @RequestBody"
          param.pos.fold(report.error(msg))(report.error(msg, _))
        }
        bodyType = Some(tpe)
        None
      }
      else {
        val kind = readParameterKind(param).getOrElse {
          val msg = s"Parameter ${param.name} in ${methodSym.name} has no HTTP annotation, defaulting to @Query"
          param.pos.fold(report.warning(msg))(report.warning(msg, _))
          ParamKind.Query
        }
        val name = annotationValue(param, kind.annotationName).filter(_.nonEmpty).getOrElse(param.name)
        Some(ParamInfo(name, tpe, kind))
      }
    }

    val returnType = methodSym.tree match {
      case defDef: DefDef => defDef.returnTpt.tpe
      case _ => TypeRepr.of[Unit]
    }
    val (isZioReturn, outputType) = unwrapReturnType(returnType)
    val consumesMediaType = annotationValue(methodSym, "Consumes").filter(_.nonEmpty).getOrElse("application/json")
    val producesMediaType = annotationValue(methodSym, "Produces").filter(_.nonEmpty).getOrElse("application/json")

    ParsedMethod(params, bodyType, consumesMediaType, isZioReturn, outputType, producesMediaType)
  }

  private def unwrapReturnType(using q: Quotes)(returnType: q.reflect.TypeRepr): (Boolean, q.reflect.TypeRepr) = {
    import q.reflect.*
    returnType.dealias.simplified match {
      case AppliedType(zioType, List(envType, errorType, successType))
           if zioType.typeSymbol == TypeRepr.of[ZIO[Any, Any, Any]].typeSymbol =>
        if (!(envType =:= TypeRepr.of[Any])) {
          report.errorAndAbort(
            s"Controller methods must return ZIO[Any, ApiError, A]. Unsupported environment: ${envType.show}"
          )
        }
        if (!(errorType <:< TypeRepr.of[ApiError])) {
          report.errorAndAbort(
            s"Controller methods must return ZIO[Any, ApiError, A]. Unsupported error type: ${errorType.show}"
          )
        }
        (true, successType)
      case _ =>
        (false, returnType)
    }
  }

  // ── Annotation utilities ───────────────────────────────────────────────────

  private def annotationValue(using q: Quotes)(sym: q.reflect.Symbol, name: String): Option[String] = {
    sym.annotations.collectFirst {
      case ann if ann.tpe.typeSymbol.name == name => extractStringArg(ann).getOrElse("")
    }
  }

  private def hasAnnotation(using q: Quotes)(sym: q.reflect.Symbol, name: String): Boolean =
    sym.annotations.exists(_.tpe.typeSymbol.name == name)

  private def readParameterKind(using q: Quotes)(sym: q.reflect.Symbol): Option[ParamKind] = {
    sym.annotations.collectFirst {
      case ann if ann.tpe.typeSymbol.name == "Path" => ParamKind.Path
      case ann if ann.tpe.typeSymbol.name == "Query" => ParamKind.Query
      case ann if ann.tpe.typeSymbol.name == "Header" => ParamKind.Header
      case ann if ann.tpe.typeSymbol.name == "Cookie" => ParamKind.Cookie
      case ann if ann.tpe.typeSymbol.name == "BearerAuth" => ParamKind.BearerAuth
    }
  }

  private def extractStringArg(using q: Quotes)(annotation: q.reflect.Term): Option[String] = {
    annotation match {
      case q.reflect.Apply(_, List(q.reflect.Literal(q.reflect.StringConstant(value)))) => Some(value)
      case _ => None
    }
  }

  private def extractIntArg(using q: Quotes)(annotation: q.reflect.Term): Option[Int] = {
    annotation match {
      case q.reflect.Apply(_, List(q.reflect.Literal(q.reflect.IntConstant(value)))) => Some(value)
      case _ => None
    }
  }

  private def normalizePath(basePath: String, methodPath: String): String = {
    (basePath.stripSuffix("/") + "/" + methodPath.stripPrefix("/"))
      .replaceAll("//+", "/")
      .nn
  }

  /** Sort key ordering more specific routes first: a 0/1 mask over path segments (0 = fixed, 1 = capture) compared
    * lexicographically, so fixed segments win at each position.
    */
  private def pathSpecificityKey(fullPath: String): String = {
    fullPath
      .split('/')
      .nn
      .toList
      .filter(_.nonEmpty)
      .map(s => if s.startsWith("{") && s.endsWith("}") then '1' else '0')
      .mkString
  }

  // ── Data model ─────────────────────────────────────────────────────────────

  private enum ParamKind {
    case Path, Query, Header, Cookie, BearerAuth

    def annotationName: String = this match {
      case Path => "Path"
      case Query => "Query"
      case Header => "Header"
      case Cookie => "Cookie"
      case BearerAuth => "BearerAuth"
    }
  }

  final private case class ParamInfo(name: String, tpe: Any, kind: ParamKind)

  final private case class ParsedMethod(
    params: List[ParamInfo],
    body: Option[Any],
    consumesMediaType: String,
    isZioReturn: Boolean,
    outputType: Any,
    producesMediaType: String,
  )
}
