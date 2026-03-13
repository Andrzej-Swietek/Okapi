package com.okapi.core.macros

import com.okapi.core.annotations.*
import com.okapi.core.http.ApiError
import com.okapi.core.http.ApiError.ApiErrorResponse
import com.okapi.core.OkapiRuntime
import scala.quoted.*
import sttp.tapir.*
import sttp.tapir.json.zio.*
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import sttp.tapir.ztapir.*
import zio.*
import zio.http.{ Response, Routes }

private[okapi] object AnnotationProcessor {

  private val HttpAnnotations = Set("Get", "Post", "Put", "Delete", "Patch")

  inline def endpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    ${ endpointsImpl[T] }

  inline def generateEndpoints[T]: List[ZServerEndpoint[T, sttp.capabilities.WebSockets]] =
    ${ endpointsImpl[T] }

  inline def swaggerRoutes[T](
    title: String,
    version: String,
  ): Routes[Any, Response] =
    ${
      swaggerRoutesImpl[T]('title, 'version)
    }

  inline def httpRoutes[T]: Routes[T, Response] =
    ${
      httpRoutesImpl[T]
    }

  private def swaggerRoutesImpl[T: Type](
    title: Expr[String],
    version: Expr[String],
  )(using q: Quotes
  ): Expr[Routes[Any, Response]] =
    '{
      val publicEndpoints = AnnotationProcessor.endpoints[T].map(_.endpoint)
      val swaggerEndpoints = SwaggerInterpreter().fromEndpoints[Task](publicEndpoints, $title, $version)
      ZioHttpInterpreter().toHttp(swaggerEndpoints)
    }

  private def httpRoutesImpl[T: Type](using Quotes): Expr[Routes[T, Response]] =
    '{
      ZioHttpInterpreter()
        .asInstanceOf[sttp.tapir.server.ziohttp.ZioHttpInterpreter[T]]
        .toHttp(
        AnnotationProcessor.endpoints[T]
          .asInstanceOf[List[ZServerEndpoint[T, sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets]]]
        )
    }

  private[core] def pathInput[T](
    name: String,
  )(using Codec[String, T, CodecFormat.TextPlain]
  ): EndpointInput.PathCapture[T] =
    EndpointInput.PathCapture(
      Some(name),
      summon[Codec[String, T, CodecFormat.TextPlain]],
      EndpointIO.Info.empty,
    )

  private[core] def queryInput[T](
    name: String,
  )(using Codec[List[String], T, CodecFormat.TextPlain]
  ): EndpointInput.Query[T] =
    EndpointInput.Query(
      name,
      None,
      summon[Codec[List[String], T, CodecFormat.TextPlain]],
      EndpointIO.Info.empty,
    )

  private[core] def headerInput[T](
    name: String,
  )(using Codec[List[String], T, CodecFormat.TextPlain]
  ): EndpointIO.Header[T] =
    EndpointIO.Header(
      name,
      summon[Codec[List[String], T, CodecFormat.TextPlain]],
      EndpointIO.Info.empty,
    )

  private[core] def cookieInput[T](
    name: String,
  )(using Codec[Option[String], T, CodecFormat.TextPlain]
  ): EndpointInput.Cookie[T] =
    EndpointInput.Cookie(
      name,
      summon[Codec[Option[String], T, CodecFormat.TextPlain]],
      EndpointIO.Info.empty,
    )

  private def endpointsImpl[T: Type](using Quotes): Expr[List[ZServerEndpoint[T, sttp.capabilities.WebSockets]]] = {
    import quotes.reflect.*

    val controllerTpe = TypeRepr.of[T]
    val controllerSym = controllerTpe.typeSymbol

    val basePath = annotationValue(controllerSym, "Controller").getOrElse("")
    val tag = annotationValue(controllerSym, "Tag")
      .filter(_.nonEmpty)
      .getOrElse(controllerSym.name)

    val endpointExprs = controllerSym.memberMethods
      .filter(method => method.annotations.exists(a => HttpAnnotations.contains(a.tpe.typeSymbol.name)))
      .map { method =>
        val methodAnnotation = method.annotations.find(a => HttpAnnotations.contains(a.tpe.typeSymbol.name)).get
        val httpMethod = methodAnnotation.tpe.typeSymbol.name
        val methodPath = extractStringArg(methodAnnotation).getOrElse("")

        buildEndpoint[T](
          controllerTpe = controllerTpe,
          methodSym = method,
          httpMethod = httpMethod,
          fullPath = normalizePath(basePath, methodPath),
          tag = tag,
          summary = annotationValue(method, "Summary"),
          description = annotationValue(method, "Description"),
        )
      }

    Expr.ofList(endpointExprs)
  }

  private def buildEndpoint[T: Type](using q: Quotes)(
    controllerTpe: q.reflect.TypeRepr,
    methodSym: q.reflect.Symbol,
    httpMethod: String,
    fullPath: String,
    tag: String,
    summary: Option[String],
    description: Option[String],
  ): Expr[ZServerEndpoint[T, sttp.capabilities.WebSockets]] = {
    import q.reflect.*

    val ParsedMethod(params, body, isZioReturn, outputType) = parseMethod(methodSym)

    var endpointTerm: Term = baseEndpointTerm(httpMethod)

    fullPath.split('/').nn.toList.filter(_.nonEmpty).foreach { segment =>
      val fixedPath = '{
        sttp.tapir.EndpointInput.FixedPath(${ Expr(segment) }, sttp.tapir.Codec.idPlain(), sttp.tapir.EndpointIO.Info.empty)
      }.asTerm
      endpointTerm = applyEndpointOperation(endpointTerm, "in", fixedPath)
    }

    params.foreach { param =>
      endpointTerm = applyEndpointOperation(endpointTerm, "in", parameterInputExpr(param).asTerm)
    }

    body.foreach { bodyType =>
      endpointTerm = applyEndpointOperation(endpointTerm, "in", jsonBodyExpr(bodyType.asInstanceOf[TypeRepr]).asTerm)
    }

    endpointTerm = applyEndpointOperation(endpointTerm, "out", outputExpr(outputType.asInstanceOf[TypeRepr]).asTerm)
    endpointTerm = applyEndpointOperation(endpointTerm, "errorOut", errorOutputExpr.asTerm)
    endpointTerm = applyEndpointOperation(endpointTerm, "tag", Expr(tag).asTerm)

    summary.foreach { value =>
      endpointTerm = applyEndpointOperation(endpointTerm, "summary", Expr(value).asTerm)
    }

    description.foreach { value =>
      endpointTerm = applyEndpointOperation(endpointTerm, "description", Expr(value).asTerm)
    }

    val logicTerm = buildServerLogic[T](
      controllerTpe = controllerTpe,
      methodSym = methodSym,
      params = params,
      body = body,
      isZioReturn = isZioReturn,
      outputType = outputType,
    )

    val endpointBaseType = endpointTerm.tpe.widenTermRefByName.baseType(TypeRepr.of[Endpoint[Any, Any, Any, Any, Any]].typeSymbol)
    val (_, inputType, errorType, endpointOutputType, _) = endpointBaseType match {
      case AppliedType(_, List(securityInput, input, errorOutput, output, capability)) =>
        (securityInput, input, errorOutput, output, capability)
      case other =>
        report.throwError(s"Unexpected endpoint type shape before attaching logic: ${other.show}")
    }

    val helper = TypeApply(
          Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "attachServerLogic"),
      List(
        Inferred(TypeRepr.of[T]),
        Inferred(inputType),
        Inferred(errorType),
        Inferred(endpointOutputType),
      ),
    )

    Apply(helper, List(endpointTerm, logicTerm)).asExprOf[ZServerEndpoint[T, sttp.capabilities.WebSockets]]
  }

  private def buildServerLogic[T: Type](using q: Quotes)(
    controllerTpe: q.reflect.TypeRepr,
    methodSym: q.reflect.Symbol,
    params: List[ParamInfo],
    body: Option[Any],
    isZioReturn: Boolean,
    outputType: Any,
  ): q.reflect.Term = {
    import q.reflect.*

    val userInputTypes = params.map(_.tpe.asInstanceOf[TypeRepr]) ++ body.toList.map(_.asInstanceOf[TypeRepr])
    val inputType = tupleType(userInputTypes)

    val zioTc = TypeRepr.of[ZIO[Any, Any, Any]] match {
      case AppliedType(tc, _) => tc
      case other              => report.throwError(s"Unexpected ZIO type shape: ${other.show}")
    }
    val mappedErrorType = TypeRepr.of[(sttp.model.StatusCode, ApiErrorResponse)]
    val successType = outputType.asInstanceOf[TypeRepr]
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
          (innerOwner, controllerTerms) => {
            val controllerTerm = controllerTerms.head.asInstanceOf[Term]
            val extractedArgs = extractArgs(inputTerm, params, body)
            val methodCall = callControllerMethod(methodSym, extractedArgs, controllerTerm)
            val zioCall =
              if (isZioReturn) methodCall
              else wrapInZioSucceed(methodCall, successType)

            mapApiError(zioCall, successType)
          },
        )

        val helper = TypeApply(
          Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "serviceWithMappedZio"),
          List(Inferred(controllerTpe), Inferred(successType)),
        )

        Apply(helper, List(controllerLambda))
      },
    )
  }

  private def extractArgs(using q: Quotes)(
    inputTerm: q.reflect.Term,
    params: List[ParamInfo],
    body: Option[Any],
  ): List[q.reflect.Term] = {
    import q.reflect.*

    val inputTypes = params.map(_.tpe.asInstanceOf[TypeRepr]) ++ body.toList.map(_.asInstanceOf[TypeRepr])

    if (inputTypes.isEmpty) Nil
    else if (inputTypes.length == 1) List(castTerm(inputTerm, inputTypes.head))
    else {
      val asProduct = TypeApply(Select.unique(inputTerm, "asInstanceOf"), List(Inferred(TypeRepr.of[Product])))
      inputTypes.zipWithIndex.map { case (tpe, idx) =>
        val element = Apply(
          Select.unique(asProduct, "productElement"),
          List(Literal(IntConstant(idx))),
        )
        castTerm(element, tpe)
      }
    }
  }

  private def callControllerMethod(using q: Quotes)(
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

  private def acceptsEmptyArgumentList(using q: Quotes)(
    methodSym: q.reflect.Symbol,
  ): Boolean = {
    import q.reflect.*

    methodSym.tree match {
      case defDef: DefDef =>
        defDef.termParamss.nonEmpty && defDef.termParamss.forall(_.params.isEmpty)
      case _              =>
        false
    }
  }

  private def wrapInZioSucceed(using q: Quotes)(
    term: q.reflect.Term,
    outputType: q.reflect.TypeRepr,
  ): q.reflect.Term = {
    import q.reflect.*

    Apply(
      TypeApply(
        Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "liftPure"),
        List(Inferred(outputType)),
      ),
      List(term),
    )
  }

  private def mapApiError(using q: Quotes)(
    zioCall: q.reflect.Term,
    successType: q.reflect.TypeRepr,
  ): q.reflect.Term = {
    import q.reflect.*

    val helper = TypeApply(
      Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "mapApiErrorEffect"),
      List(Inferred(TypeRepr.of[Any]), Inferred(successType)),
    )

    Apply(helper, List(zioCall))
  }

  private def parameterInputExpr(using q: Quotes)(param: ParamInfo): Expr[EndpointInput[?]] =
    param.tpe.asInstanceOf[q.reflect.TypeRepr].asType match {
      case '[t] =>
        param.kind match {
          case ParamKind.Path =>
            Expr.summon[Codec[String, t, CodecFormat.TextPlain]] match {
              case Some(codec) =>
                '{ EndpointInput.PathCapture(Some(${ Expr(param.name) }), $codec, EndpointIO.Info.empty) }
              case None        =>
                q.reflect.report.throwError(s"Missing Codec[String, ${param.tpe.asInstanceOf[q.reflect.TypeRepr].show}, CodecFormat.TextPlain] for @Path parameter ${param.name}")
            }
          case ParamKind.Query =>
            Expr.summon[Codec[List[String], t, CodecFormat.TextPlain]] match {
              case Some(codec) =>
                '{ EndpointInput.Query(${ Expr(param.name) }, None, $codec, EndpointIO.Info.empty) }
              case None        =>
                q.reflect.report.throwError(s"Missing Codec[List[String], ${param.tpe.asInstanceOf[q.reflect.TypeRepr].show}, CodecFormat.TextPlain] for @Query parameter ${param.name}")
            }
          case ParamKind.Header =>
            Expr.summon[Codec[List[String], t, CodecFormat.TextPlain]] match {
              case Some(codec) =>
                '{ EndpointIO.Header(${ Expr(param.name) }, $codec, EndpointIO.Info.empty) }
              case None        =>
                q.reflect.report.throwError(s"Missing Codec[List[String], ${param.tpe.asInstanceOf[q.reflect.TypeRepr].show}, CodecFormat.TextPlain] for @Header parameter ${param.name}")
            }
          case ParamKind.Cookie =>
            Expr.summon[Codec[Option[String], t, CodecFormat.TextPlain]] match {
              case Some(codec) =>
                '{ EndpointInput.Cookie(${ Expr(param.name) }, $codec, EndpointIO.Info.empty) }
              case None        =>
                q.reflect.report.throwError(s"Missing Codec[Option[String], ${param.tpe.asInstanceOf[q.reflect.TypeRepr].show}, CodecFormat.TextPlain] for @Cookie parameter ${param.name}")
            }
        }
    }

  private def jsonBodyExpr(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[EndpointIO.Body[?, ?]] =
    tpe.asType match {
      case '[t] =>
        val schemaExpr = Expr.summon[Schema[t]].orElse {
          Expr.summon[scala.deriving.Mirror.Of[t]].map { mirror =>
            '{ Schema.derived[t](using sttp.tapir.generic.Configuration.default, $mirror) }
          }
        }
        (Expr.summon[zio.json.JsonCodec[t]], schemaExpr) match {
            case (Some(codec), Some(schema)) => '{ OkapiRuntime.jsonBodyInput[t]($codec, $schema) }
          case (None, _)                   => q.reflect.report.throwError(s"Missing zio.json.JsonCodec[${tpe.show}] for request body")
          case (_, None)                   => q.reflect.report.throwError(s"Missing sttp.tapir.Schema[${tpe.show}] for request body")
        }
    }

  private def outputExpr(using q: Quotes)(tpe: q.reflect.TypeRepr): Expr[EndpointOutput[?]] =
    if (tpe =:= q.reflect.TypeRepr.of[Unit]) '{ sttp.tapir.emptyOutput }
    else if (tpe <:< q.reflect.TypeRepr.of[String]) '{ sttp.tapir.stringBody }
    else {
      tpe.asType match {
        case '[t] =>
          val schemaExpr = Expr.summon[Schema[t]].orElse {
            Expr.summon[scala.deriving.Mirror.Of[t]].map { mirror =>
              '{ Schema.derived[t](using sttp.tapir.generic.Configuration.default, $mirror) }
            }
          }
          (Expr.summon[zio.json.JsonCodec[t]], schemaExpr) match {
            case (Some(codec), Some(schema)) => '{ OkapiRuntime.jsonBodyOutput[t]($codec, $schema) }
            case (None, _)                   => q.reflect.report.throwError(s"Missing zio.json.JsonCodec[${tpe.show}] for response body")
            case (_, None)                   => q.reflect.report.throwError(s"Missing sttp.tapir.Schema[${tpe.show}] for response body")
          }
      }
    }

  private def errorOutputExpr(using q: Quotes): Expr[EndpointOutput[(sttp.model.StatusCode, ApiErrorResponse)]] =
    Expr.summon[Schema[ApiErrorResponse]]
      .orElse(Expr.summon[scala.deriving.Mirror.Of[ApiErrorResponse]].map { mirror =>
        '{ Schema.derived[ApiErrorResponse](using sttp.tapir.generic.Configuration.default, $mirror) }
      })
      .map { schema =>
        '{
          sttp.tapir.statusCode.and(
            OkapiRuntime.jsonBodyOutput[ApiErrorResponse](
              summon[zio.json.JsonCodec[ApiErrorResponse]],
              $schema,
            ),
          )
        }
      }
      .getOrElse(q.reflect.report.throwError("Missing sttp.tapir.Schema[ApiErrorResponse] for error output"))

  private def applyEndpointOperation(using q: Quotes)(
    qualifier: q.reflect.Term,
    methodName: String,
    argument: q.reflect.Term,
  ): q.reflect.Term = {
    import q.reflect.*

    def endpointTypeParts(term: Term): (TypeRepr, TypeRepr, TypeRepr, TypeRepr, TypeRepr) =
      term.tpe.widenTermRefByName match {
        case AppliedType(_, List(securityInput, input, errorOutput, output, capability)) =>
          (securityInput, input, errorOutput, output, capability)
        case other =>
          report.throwError(s"Unexpected endpoint type shape for ${term.show}: ${other.show}")
      }

    def transputValueType(tpe: TypeRepr): TypeRepr =
      tpe.widenTermRefByName match {
        case widened =>
          val inputBase = widened.baseType(TypeRepr.of[EndpointInput[Any]].typeSymbol)
          val outputBase = widened.baseType(TypeRepr.of[EndpointOutput[Any]].typeSymbol)

          inputBase match {
            case AppliedType(_, List(valueType)) => valueType
            case _ =>
              outputBase match {
                case AppliedType(_, List(valueType)) => valueType
                case _                               => report.throwError(s"Unexpected transput type shape: ${widened.show}")
              }
          }
      }

    def flattenTupleTypes(tpe: TypeRepr): List[TypeRepr] =
      if (tpe =:= TypeRepr.of[Unit] || tpe =:= TypeRepr.of[EmptyTuple]) Nil
      else {
        val tupleCons = TypeRepr.of[*:[Any, EmptyTuple]] match {
          case AppliedType(tc, _) => tc
          case other              => report.throwError(s"Unexpected tuple cons shape: ${other.show}")
        }

        tpe.dealias match {
          case AppliedType(cons, List(head, tail)) if cons =:= tupleCons =>
            head :: flattenTupleTypes(tail)
          case other =>
            List(other)
        }
      }

    def concatValueTypes(left: TypeRepr, right: TypeRepr): TypeRepr =
      tupleType(flattenTupleTypes(left) ++ flattenTupleTypes(right))

    def summonParamConcat(left: TypeRepr, right: TypeRepr): Term = {
      val paramConcatTpe = TypeRepr.of[sttp.tapir.typelevel.ParamConcat[Any, Any]] match {
        case AppliedType(tc, _) => AppliedType(tc, List(left, right))
        case other              => report.throwError(s"Unexpected ParamConcat type shape: ${other.show}")
      }

      Implicits.search(paramConcatTpe) match {
        case success: ImplicitSearchSuccess => success.tree
        case failure: ImplicitSearchFailure =>
          report.throwError(s"Cannot summon ParamConcat[${left.show}, ${right.show}]: ${failure.explanation}")
      }
    }

    methodName match {
      case "in" =>
        val (securityInput, currentInput, errorOutput, output, capability) = endpointTypeParts(qualifier)
        val nextInput = transputValueType(argument.tpe)
        val combinedInput = concatValueTypes(currentInput, nextInput)
        val concat = summonParamConcat(currentInput, transputValueType(argument.tpe))
        val helper = TypeApply(
          Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "addInput"),
          List(
            Inferred(securityInput),
            Inferred(currentInput),
            Inferred(errorOutput),
            Inferred(output),
            Inferred(capability),
            Inferred(nextInput),
            Inferred(combinedInput),
          ),
        )
        Apply(Apply(helper, List(qualifier, argument)), List(concat))
      case "out" =>
        val (securityInput, input, errorOutput, currentOutput, capability) = endpointTypeParts(qualifier)
        val nextOutput = transputValueType(argument.tpe)
        val combinedOutput = concatValueTypes(currentOutput, nextOutput)
        val concat = summonParamConcat(currentOutput, nextOutput)
        val helper = TypeApply(
          Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "addOutput"),
          List(
            Inferred(securityInput),
            Inferred(input),
            Inferred(errorOutput),
            Inferred(currentOutput),
            Inferred(capability),
            Inferred(nextOutput),
            Inferred(combinedOutput),
          ),
        )
        Apply(Apply(helper, List(qualifier, argument)), List(concat))
      case "errorOut" =>
        val (securityInput, input, currentErrorOutput, output, capability) = endpointTypeParts(qualifier)
        val nextErrorOutput = transputValueType(argument.tpe)
        val combinedErrorOutput = concatValueTypes(currentErrorOutput, nextErrorOutput)
        val concat = summonParamConcat(currentErrorOutput, nextErrorOutput)
        val helper = TypeApply(
          Select.unique(Ref(Symbol.requiredModule("com.okapi.core.OkapiRuntime")), "addErrorOutput"),
          List(
            Inferred(securityInput),
            Inferred(input),
            Inferred(currentErrorOutput),
            Inferred(output),
            Inferred(capability),
            Inferred(nextErrorOutput),
            Inferred(combinedErrorOutput),
          ),
        )
        Apply(Apply(helper, List(qualifier, argument)), List(concat))
      case _ =>
        val ownerSymbol = qualifier.tpe.widenTermRefByName.typeSymbol
        val method = ownerSymbol.memberMethod(methodName).find { candidate =>
          candidate.paramSymss.find(_.headOption.exists(_.isTerm)).exists(_.size == 1)
        }.getOrElse {
          report.throwError(s"Cannot resolve method $methodName for ${qualifier.tpe.show}")
        }
        Apply(Select(qualifier, method), List(argument))
    }
  }

  private def baseEndpointTerm(using q: Quotes)(httpMethod: String): q.reflect.Term = {
    import q.reflect.*

    httpMethod match {
      case "Get"    => '{ sttp.tapir.endpoint.get }.asTerm
      case "Post"   => '{ sttp.tapir.endpoint.post }.asTerm
      case "Put"    => '{ sttp.tapir.endpoint.put }.asTerm
      case "Delete" => '{ sttp.tapir.endpoint.delete }.asTerm
      case "Patch"  => '{ sttp.tapir.endpoint.patch }.asTerm
      case other    => q.reflect.report.throwError(s"Unsupported HTTP method annotation: $other")
    }
  }

  private def castTerm(using q: Quotes)(
    term: q.reflect.Term,
    tpe: q.reflect.TypeRepr,
  ): q.reflect.Term = {
    import q.reflect.*
    TypeApply(Select.unique(term, "asInstanceOf"), List(Inferred(tpe)))
  }

  private def tupleType(using q: Quotes)(types: List[q.reflect.TypeRepr]): q.reflect.TypeRepr = {
    import q.reflect.*

    types match {
      case Nil          => TypeRepr.of[Unit]
      case single :: Nil => single
      case many         =>
        val tupleCons = TypeRepr.of[*:[Any, EmptyTuple]] match {
          case AppliedType(tc, _) => tc
          case other              => report.throwError(s"Unexpected tuple cons shape: ${other.show}")
        }
        many.foldRight(TypeRepr.of[EmptyTuple]) { (head, tail) =>
          AppliedType(tupleCons, List(head, tail))
        }
    }
  }

  private def httpVerbSelector(httpMethod: String): String =
    httpMethod match {
      case "Get"    => "get"
      case "Post"   => "post"
      case "Put"    => "put"
      case "Delete" => "delete"
      case "Patch"  => "patch"
      case other    => other.toLowerCase.nn
    }

  private def parseMethod(using q: Quotes)(methodSym: q.reflect.Symbol): ParsedMethod = {
    import q.reflect.*

    val allParams = methodSym.paramSymss.flatten
    var bodyType: Option[TypeRepr] = None

    val params = allParams.flatMap { param =>
      val tpe = param.tree match {
        case valueDef: ValDef => valueDef.tpt.tpe
        case _                => param.termRef.widen
      }

      if (hasAnnotation(param, "RequestBody")) {
        if (bodyType.nonEmpty) {
          param.pos match {
            case Some(position) => report.error(s"Method ${methodSym.name} can declare only one @RequestBody", position)
            case None      => report.error(s"Method ${methodSym.name} can declare only one @RequestBody")
          }
        }
        bodyType = Some(tpe)
        None
      }
      else {
        val kind = readParameterKind(param).getOrElse {
          param.pos match {
            case Some(position) =>
              report.warning(
                s"Parameter ${param.name} in ${methodSym.name} has no HTTP annotation, defaulting to @Query",
                position,
              )
            case None      =>
              report.warning(
                s"Parameter ${param.name} in ${methodSym.name} has no HTTP annotation, defaulting to @Query",
              )
          }
          ParamKind.Query
        }

        val name = annotationValue(param, annotationName(kind)).getOrElse(param.name)
        Some(ParamInfo(name, tpe, kind))
      }
    }

    val returnType = methodReturnType(methodSym)
    val (isZioReturn, outputType) = unwrapReturnType(returnType)

    ParsedMethod(params, bodyType, isZioReturn, outputType)
  }

  private def methodReturnType(using q: Quotes)(methodSym: q.reflect.Symbol): q.reflect.TypeRepr = {
    import q.reflect.*

    methodSym.tree match {
      case defDef: DefDef => defDef.returnTpt.tpe
      case _              => TypeRepr.of[Unit]
    }
  }

  private def unwrapReturnType(
    using q: Quotes
  )(
    returnType: q.reflect.TypeRepr,
  ): (Boolean, q.reflect.TypeRepr) = {
    import q.reflect.*

    returnType.dealias.simplified match {
      case AppliedType(zioType, List(envType, errorType, successType))
          if zioType.typeSymbol == TypeRepr.of[ZIO[Any, Any, Any]].typeSymbol =>
        if (!(envType =:= TypeRepr.of[Any])) {
          report.error(
            s"Controller methods must return ZIO[Any, ApiError, A]. Unsupported environment: ${envType.show}",
          )
        }
        if (!(errorType <:< TypeRepr.of[ApiError])) {
          report.error(
            s"Controller methods must return ZIO[Any, ApiError, A]. Unsupported error type: ${errorType.show}",
          )
        }
        (true, successType)

      case _ =>
        (false, returnType)
    }
  }

  private def normalizePath(basePath: String, methodPath: String): String = {
    val base = Option(basePath).getOrElse("")
    val method = Option(methodPath).getOrElse("")

    (base.stripSuffix("/") + "/" + method.stripPrefix("/"))
      .replaceAll("//+", "/")
      .nn
  }

  private def extractStringArg(using q: Quotes)(annotation: q.reflect.Term): Option[String] = {
    import q.reflect.*

    annotation match {
      case Apply(_, List(Literal(StringConstant(value)))) => Some(value)
      case _                                              => None
    }
  }

  private def annotationValue(using q: Quotes)(
    symbol: q.reflect.Symbol,
    annotationName: String,
  ): Option[String] = {
    import q.reflect.*

    symbol.annotations.collectFirst {
      case annotation if annotation.tpe.typeSymbol.name == annotationName =>
        extractStringArg(annotation).getOrElse("")
    }
  }

  private def hasAnnotation(using q: Quotes)(
    symbol: q.reflect.Symbol,
    annotationName: String,
  ): Boolean =
    symbol.annotations.exists(_.tpe.typeSymbol.name == annotationName)

  private def readParameterKind(using q: Quotes)(
    symbol: q.reflect.Symbol,
  ): Option[ParamKind] =
    symbol.annotations.collectFirst {
      case annotation if annotation.tpe.typeSymbol.name == "Path"   => ParamKind.Path
      case annotation if annotation.tpe.typeSymbol.name == "Query"  => ParamKind.Query
      case annotation if annotation.tpe.typeSymbol.name == "Header" => ParamKind.Header
      case annotation if annotation.tpe.typeSymbol.name == "Cookie" => ParamKind.Cookie
    }

  private def annotationName(kind: ParamKind): String =
    kind match {
      case ParamKind.Path   => "Path"
      case ParamKind.Query  => "Query"
      case ParamKind.Header => "Header"
      case ParamKind.Cookie => "Cookie"
    }

  private enum ParamKind {
    case Path, Query, Header, Cookie
  }

  private final case class ParamInfo(
    name: String,
    tpe: Any,
    kind: ParamKind,
  )

  private final case class ParsedMethod(
    params: List[ParamInfo],
    body: Option[Any],
    isZioReturn: Boolean,
    outputType: Any,
  )
}
