package io.okapi.core

import zio.{ IO, RIO, Scope, Task, Trace, UIO, URIO, ZIO, ZLayer }
import zio.http.{ Request, Response, Routes, Status, URL }
import zio.json.JsonCodec
import zio.test.*

import java.util.concurrent.ConcurrentLinkedQueue

import io.okapi.core.annotations.{ Controller, Get, Header, Path, Post, Query, RequestBody, WebSocket }
import io.okapi.core.http.{ ApiError, ApiErrorException, ApiResponse }
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import sttp.tapir.server.ziohttp.ZioHttpServerOptions

/** Controller methods returning ZIO effects, served over ZIO HTTP: error channels, environments, scopes, signature and
  * annotation shapes, JSON, per-call responses, streams, WebSockets and layers.
  */
object ZioEffectsSpec extends ZIOSpecDefault {

  final case class Msg(text: String) derives JsonCodec

  final class Greeter {
    def greet(name: String): String = s"hello $name"
  }

  final class Prefix(val value: String)

  @Controller("/fx")
  final class EffectsController {

    @Get("/io")
    def io: IO[ApiError, Msg] = ZIO.succeed(Msg("io"))

    @Get("/not-found")
    def notFound: IO[ApiError.NotFound, Msg] = ZIO.fail(ApiError.NotFound("no such thing"))

    @Get("/uio")
    def uio: UIO[Msg] = ZIO.succeed(Msg("uio"))

    @Get("/task")
    def task: Task[Msg] = ZIO.attempt(Msg("task"))

    @Get("/task-failure")
    def taskFailure: Task[Msg] = ZIO.fail(new IllegalStateException("db password is hunter2"))

    @Get("/task-api-error")
    def taskApiError: Task[Msg] = ZIO.fail(ApiErrorException(ApiError.Forbidden("not yours")))

    @Get("/throws")
    def throws: Task[Msg] = throw new IllegalStateException("thrown before the effect: secret")

    @Get("/throws-api-error")
    def throwsApiError: IO[ApiError, Msg] = throw ApiErrorException(ApiError.NotFound("thrown nf"))

    @Get("/defect")
    def defect: UIO[Msg] = ZIO.die(new IllegalStateException("bug"))

    @Get("/died-api-error")
    def diedApiError: UIO[Msg] = ZIO.fail(ApiErrorException(ApiError.NotFound("gone"))).orDie

    @Get("/union")
    def union(@Query("fail") fail: String): ZIO[Any, ApiError | Throwable, Msg] = {
      fail match {
        case "api" => ZIO.fail(ApiError.Conflict("taken"))
        case "throwable" => ZIO.fail(new RuntimeException("boom"))
        case _ => ZIO.succeed(Msg("union"))
      }
    }
  }

  @Controller("/env")
  final class EnvController {

    @Get("/greet/{name}")
    def greet(@Path("name") name: String): URIO[Greeter, Msg] = ZIO.serviceWith[Greeter](g => Msg(g.greet(name)))

    @Get("/prefixed")
    def prefixed: ZIO[Greeter & Prefix, ApiError, Msg] =
      ZIO.serviceWith[Prefix](_.value).zipWith(ZIO.serviceWith[Greeter](_.greet("you")))((p, g) => Msg(s"$p $g"))

    @Get("/self")
    def self: RIO[EnvController, Msg] = ZIO.succeed(Msg("self"))
  }

  final class ResourceLog {
    val events = new ConcurrentLinkedQueue[String]()
    def all: List[String] = events.asScala.toList
  }

  @Controller("/scoped")
  final class ScopedController(log: ResourceLog) {

    @Get("/resource")
    def resource: ZIO[Scope, ApiError, Msg] = {
      ZIO
        .acquireRelease(ZIO.succeed(log.events.add("acquire")))(_ => ZIO.succeed(log.events.add("release")))
        .as(Msg(s"inside: ${log.all.mkString(",")}"))
    }
  }

  @Controller("/shapes")
  final class ShapesController {

    @Get(path = "/named")
    def named: UIO[Msg] = ZIO.succeed(Msg("named"))

    @Get("/curried/{a}")
    def curried(@Path("a") a: Int)(@Query("b") b: Int): UIO[Msg] = ZIO.succeed(Msg(s"${a + b}"))

    @Get("/traced")
    def traced(@Query("q") q: String)(using trace: Trace): UIO[Msg] = ZIO.succeed(Msg(s"$q:traced"))(using trace)

    @Get("/overloaded")
    def overloaded(@Query("x") x: Int): UIO[Msg] = ZIO.succeed(Msg(s"x=$x"))

    def overloaded(x: String): String = x

    @Get("/wide/{p1}/{p2}")
    def wide(
      @Path("p1") p1: Int,
      @Path("p2") p2: Int,
      @Query("a1") a1: Int,
      @Query("a2") a2: Int,
      @Query("a3") a3: Int,
      @Query("a4") a4: Int,
      @Query("a5") a5: Int,
      @Query("a6") a6: Int,
      @Query("a7") a7: Int,
      @Query("a8") a8: Int,
      @Query("a9") a9: Int,
      @Query("a10") a10: Int,
      @Query("a11") a11: Int,
      @Query("a12") a12: Int,
      @Query("a13") a13: Int,
      @Query("a14") a14: Int,
      @Query("a15") a15: Int,
      @Query("a16") a16: Int,
      @Query("a17") a17: Int,
      @Query("a18") a18: Int,
      @Query("a19") a19: Int,
      @Query("a20") a20: Int,
      @Query("a21") a21: Int,
      @Query("a22") a22: Int,
      @Query("a23") a23: Int,
      @Query("a24") a24: Int,
      @Query("a25") a25: Int,
      @Header("x-last") last: String,
    ): UIO[Msg] = {
      val queries = List(
        a1,
        a2,
        a3,
        a4,
        a5,
        a6,
        a7,
        a8,
        a9,
        a10,
        a11,
        a12,
        a13,
        a14,
        a15,
        a16,
        a17,
        a18,
        a19,
        a20,
        a21,
        a22,
        a23,
        a24,
        a25,
      )
      ZIO.succeed(Msg(s"$p1/$p2:${queries.mkString(",")}:$last"))
    }
  }

  final class Store[A]()

  final case class User(id: Int)
  final case class Book(id: Int)

  @Controller("/stores")
  final class StoresController(users: Store[User], books: Store[Book]) {

    @Get("/same")
    def same: UIO[Msg] = ZIO.succeed(Msg(s"${(users: AnyRef) ne books}"))
  }

  sealed trait Payment derives io.okapi.core.json.JsoniterCodec
  final case class Card(number: String) extends Payment
  final case class Transfer(iban: String) extends Payment

  @Controller("/payments")
  final class PaymentController {
    @Post("")
    def pay(@RequestBody payment: Payment): UIO[Payment] = ZIO.succeed(payment)
  }

  final case class Readiness(database: String) derives io.okapi.core.json.JsoniterCodec

  @Controller("/responses")
  final class ResponsesController {

    @Get("/image/{name}")
    def image(@Path("name") name: String): ApiResponse[Array[Byte]] = {
      val mediaType = {
        if name.endsWith(".svg") then sttp.model.MediaType.unsafeParse("image/svg+xml")
        else sttp.model.MediaType.ImagePng
      }
      ApiResponse(Array[Byte](1, 2, 3)).withContentType(mediaType)
    }

    @Get("/ready")
    def ready(@Query("up") up: Boolean): UIO[ApiResponse[Readiness]] = {
      ZIO.succeed {
        if up then ApiResponse(Readiness("healthy"))
        else ApiResponse(Readiness("unreachable")).withStatus(sttp.model.StatusCode.ServiceUnavailable)
      }
    }

    @Post("/session")
    def session: ApiResponse[Unit] = ApiResponse(()).withHeader("Mcp-Session-Id", "abc")

    @Get("/stream")
    def stream: ApiResponse[zio.stream.ZStream[Any, Throwable, Byte]] =
      ApiResponse(zio.stream.ZStream.fromIterable("hello".getBytes.nn)).withHeader("Content-Length", "5")
  }

  final case class ChatIn(text: String) derives io.okapi.core.json.JsoniterCodec
  final case class ChatOut(echo: String) derives io.okapi.core.json.JsoniterCodec

  @Controller("/ws")
  final class WsOptionsController {

    @WebSocket("/typed", pingIntervalSeconds = 30)
    def typed: WsPipe[ChatIn, ChatOut] = _.map(in => ChatOut(in.text))

    @WebSocket(path = "/silent", pingIntervalSeconds = 0)
    def silent: WsPipe[String, String] = identity

    @WebSocket("/default")
    def default: WsPipe[Array[Byte], Array[Byte]] = identity
  }

  @Controller("/streams")
  final class StreamsController {

    @Get("/events")
    def events: zio.stream.ZStream[Any, Throwable, sttp.model.sse.ServerSentEvent] = {
      zio.stream.ZStream(
        sttp.model.sse.ServerSentEvent(data = Some("one"), eventType = Some("tick")),
        sttp.model.sse.ServerSentEvent(data = Some("two")),
      )
    }

    @Get("/csv")
    @io.okapi.core.annotations.Produces("text/csv")
    def csv: zio.stream.ZStream[Any, Throwable, Byte] = zio.stream.ZStream.fromIterable("a,b\n".getBytes.nn)
  }

  @Controller("/chat")
  final class ChatController {

    @Get("/{room}")
    def room(@Path("room") room: String): UIO[Msg] = ZIO.succeed(Msg(room))

    @WebSocket("/live")
    def live: WsPipe[String, String] = identity
  }

  final class RandomController(random: zio.Random) {
    @Get("/random")
    def next: UIO[Msg] = random.nextInt.map(i => Msg(i.toString))
  }

  final case class AppConfig(name: String)

  object AppConfig {
    given zio.Config[AppConfig] = zio.Config.string("okapi_test_missing_key").map(AppConfig(_))
  }

  final class Hooked extends ZLayer.Derive.Scoped[Any, String] {
    def scoped(using Trace): ZIO[Scope, String, Any] = ZIO.fail("hook failed")
  }

  @Controller("/config")
  final class ConfigController(config: AppConfig) {
    @Get("/name")
    def name: UIO[Msg] = ZIO.succeed(Msg(config.name))
  }

  trait Clock {
    def now: Long
  }

  @Controller("/time")
  final class TimeController(clock: Clock) {

    @Get("/now")
    def now: UIO[Msg] = ZIO.succeed(Msg(clock.now.toString))
  }

  private def get[R](routes: Routes[R, Response], path: String): ZIO[R, Nothing, (Status, String)] = {
    ZIO.scoped[R] {
      routes.runZIO(Request.get(URL.decode(path).toOption.get)).flatMap(r => r.body.asString.orDie.map(r.status -> _))
    }
  }

  private def webSocketBody(output: sttp.tapir.EndpointOutput[?])
    : Option[sttp.tapir.WebSocketBodyOutput[?, ?, ?, ?, ?]] = {
    output match {
      case sttp.tapir.EndpointOutput.WebSocketBodyWrapper(ws) => Some(ws)
      case sttp.tapir.EndpointOutput.Pair(left, right, _, _) => webSocketBody(left).orElse(webSocketBody(right))
      case sttp.tapir.EndpointOutput.MappedPair(pair, _) => webSocketBody(pair)
      case _ => None
    }
  }

  private val fx = Okapi.httpRoutes[EffectsController]

  private def fxGet(path: String) = get(fx, path).provide(ZLayer.succeed(EffectsController()))

  override def spec: Spec[TestEnvironment & Scope, Any] = {
    suite("ZioEffectsSpec")(
      suite("error channels")(
        test("IO[ApiError, A] and UIO[A] succeed") {
          for {
            io <- fxGet("/fx/io")
            uio <- fxGet("/fx/uio")
          } yield assertTrue(io == (Status.Ok, """{"text":"io"}"""), uio == (Status.Ok, """{"text":"uio"}"""))
        },
        test("a specific ApiError subtype maps to its status") {
          fxGet("/fx/not-found").map { (status, body) =>
            assertTrue(status == Status.NotFound, body.contains("no such thing"))
          }
        },
        test("Task[A] succeeds") {
          fxGet("/fx/task").map(r => assertTrue(r == (Status.Ok, """{"text":"task"}""")))
        },
        test("a Throwable becomes a 500 without leaking its message, and is logged") {
          for {
            (status, body) <- fxGet("/fx/task-failure")
            logs <- ZTestLogger.logOutput
          } yield assertTrue(
            status == Status.InternalServerError,
            body.contains(ZioBackend.InternalErrorMessage),
            !body.contains("hunter2"),
            logs.exists(_.message().contains("Unhandled failure")),
          )
        },
        test("an ApiErrorException in a Task maps to its ApiError") {
          fxGet("/fx/task-api-error")
            .map((status, body) => assertTrue(status == Status.Forbidden, body.contains("not yours")))
        },
        test("ZIO[Any, ApiError | Throwable, A] handles both kinds of failure") {
          for {
            ok <- fxGet("/fx/union?fail=no")
            api <- fxGet("/fx/union?fail=api")
            throwable <- fxGet("/fx/union?fail=throwable")
          } yield assertTrue(
            ok._1 == Status.Ok,
            api._1 == Status.Conflict,
            throwable._1 == Status.InternalServerError,
          )
        },
        test("a method throwing while building its effect is classified like a failure") {
          for {
            thrown <- fxGet("/fx/throws")
            thrownApi <- fxGet("/fx/throws-api-error")
          } yield assertTrue(
            thrown._1 == Status.InternalServerError,
            !thrown._2.contains("secret"),
            thrownApi._1 == Status.NotFound,
          )
        },
        test("a defect becomes a logged 500 without leaking its message") {
          for {
            (status, body) <- fxGet("/fx/defect")
            logs <- ZTestLogger.logOutput
          } yield assertTrue(
            status == Status.InternalServerError,
            body.contains(ZioBackend.InternalErrorMessage),
            !body.contains("bug"),
            logs.exists(_.message().contains("Unhandled failure")),
          )
        },
        test("an ApiErrorException died with (e.g. via .orDie) maps to its ApiError") {
          fxGet("/fx/died-api-error")
            .map((status, body) => assertTrue(status == Status.NotFound, body.contains("gone")))
        },
        test("an unsupported error type is a compile error") {
          typeCheck {
            """
            @Controller("/bad") final class Bad { @Get("/x") def x: IO[String, Msg] = ZIO.fail("no") }
            Okapi.endpoints[Bad]
            """
          }.map(result => assertTrue(result.left.exists(_.contains("ZIO[R, ApiError | Throwable, A]"))))
        },
      ),
      suite("environment")(
        test("the routes require the controller plus every method's environment") {
          val routes: Routes[EnvController & Greeter & Prefix, Response] = Okapi.httpRoutes[EnvController]
          for {
            greet <- get(routes, "/env/greet/okapi")
            prefixed <- get(routes, "/env/prefixed")
            self <- get(routes, "/env/self")
          } yield assertTrue(
            greet == (Status.Ok, """{"text":"hello okapi"}"""),
            prefixed == (Status.Ok, """{"text":"dear hello you"}"""),
            self._1 == Status.Ok,
          )
        }.provide(ZLayer.succeed(EnvController()), ZLayer.succeed(Greeter()), ZLayer.succeed(Prefix("dear"))),
        test("a missing environment is a compile error") {
          typeCheck("val r: Routes[EnvController, Response] = Okapi.httpRoutes[EnvController]")
            .map(result => assertTrue(result.isLeft))
        },
        test("a controller without extra requirements keeps its plain type") {
          val endpoints: List[sttp.tapir.ztapir.ZServerEndpoint[EffectsController, sttp.capabilities.WebSockets]] =
            Okapi.endpoints[EffectsController]
          assertTrue(endpoints.size == 11)
        },
        test("routes over several controllers require the union of their environments") {
          val routes: Routes[EffectsController & EnvController & Greeter & Prefix, Response] =
            Okapi.routes[(EffectsController, EnvController)]
          get(routes, "/env/greet/both")
            .zipWith(get(routes, "/fx/io"))((env, fx) => assertTrue(env._1 == Status.Ok, fx._1 == Status.Ok))
            .provide(
              ZLayer.succeed(EffectsController()),
              ZLayer.succeed(EnvController()),
              ZLayer.succeed(Greeter()),
              ZLayer.succeed(Prefix("")),
            )
        },
      ),
      suite("scope")(
        test("a Scope requirement is not propagated to the routes") {
          val routes: Routes[ScopedController, Response] = Okapi.httpRoutes[ScopedController]
          assertTrue(routes.routes.nonEmpty)
        },
        test("every request gets its own scope, closed after the method completes") {
          val log = ResourceLog()
          val routes = Okapi.httpRoutes[ScopedController]
          (for {
            first <- get(routes, "/scoped/resource")
            second <- get(routes, "/scoped/resource")
          } yield assertTrue(
            first == (Status.Ok, """{"text":"inside: acquire"}"""),
            second == (Status.Ok, """{"text":"inside: acquire,release,acquire"}"""),
            log.all == List("acquire", "release", "acquire", "release"),
          )).provide(ZLayer.succeed(ScopedController(log)))
        },
      ),
      suite("signature and annotation shapes")(
        test("named annotation arguments, curried and using clauses, overloads") {
          val routes = Okapi.httpRoutes[ShapesController]
          (for {
            named <- get(routes, "/shapes/named")
            curried <- get(routes, "/shapes/curried/2?b=40")
            traced <- get(routes, "/shapes/traced?q=t")
            overloaded <- get(routes, "/shapes/overloaded?x=7")
          } yield assertTrue(
            named == (Status.Ok, """{"text":"named"}"""),
            curried == (Status.Ok, """{"text":"42"}"""),
            traced == (Status.Ok, """{"text":"t:traced"}"""),
            overloaded == (Status.Ok, """{"text":"x=7"}"""),
          )).provide(ZLayer.succeed(ShapesController()))
        },
        test("more than 22 inputs (Tapir flattens at most 22): served over HTTP in declaration order") {
          val routes = Okapi.httpRoutes[ShapesController]
          val query = (1 to 25).map(i => s"a$i=$i").mkString("&")
          val request = Request.get(URL.decode(s"/shapes/wide/100/200?$query").toOption.get).addHeader("x-last", "end")
          ZIO
            .scoped(routes.runZIO(request).flatMap(_.body.asString))
            .map(body => assertTrue(body == s"""{"text":"100/200:${(1 to 25).mkString(",")}:end"}"""))
            .provide(ZLayer.succeed(ShapesController()))
        },
        test("a non-literal annotation argument is a compile error") {
          typeCheck {
            """
            object Paths { var base = "/x" }
            @Controller("/c") final class C { @Get(Paths.base) def x: UIO[Msg] = ZIO.succeed(Msg("")) }
            Okapi.endpoints[C]
            """
          }.map(result => assertTrue(result.left.exists(_.contains("expects a string literal or constant argument"))))
        },
        test("conflicting parameter annotations are a compile error") {
          typeCheck {
            """
            @Controller("/c") final class C { @Get("/{a}") def x(@Path("a") @Query("a") a: Int): UIO[Msg] = ZIO.succeed(Msg("")) }
            Okapi.endpoints[C]
            """
          }.map(result => assertTrue(result.left.exists(_.contains("conflicting annotations"))))
        },
      ),
      suite("JSON")(
        test("a sealed hierarchy round-trips over HTTP and is documented with its discriminator") {
          val yaml = Okapi.openApiYaml[Tuple1[PaymentController]]("Payments", "1")
          val request = Request
            .post(URL.decode("/payments").toOption.get, zio.http.Body.fromString("""{"type":"Card","number":"4111"}"""))
            .addHeader("Content-Type", "application/json")
          ZIO
            .scoped(Okapi.httpRoutes[PaymentController].runZIO(request).flatMap(_.body.asString))
            .map { body =>
              assertTrue(
                body == """{"type":"Card","number":"4111"}""",
                yaml.contains("propertyName: type"),
                yaml.contains("Card: '#/components/schemas/Card'"),
              )
            }
            .provide(ZLayer.succeed(PaymentController()))
        }
      ),
      suite("per-call responses")(
        test("status, headers and Content-Type chosen per call") {
          val routes = Okapi.httpRoutes[ResponsesController]
          def run(request: Request) = ZIO.scoped(routes.runZIO(request).flatMap(r => r.body.asString.map(r -> _)))
          (for {
            (png, _) <- run(Request.get(URL.decode("/responses/image/a.png").toOption.get))
            (svg, _) <- run(Request.get(URL.decode("/responses/image/a.svg").toOption.get))
            (up, upBody) <- run(Request.get(URL.decode("/responses/ready?up=true").toOption.get))
            (down, downBody) <- run(Request.get(URL.decode("/responses/ready?up=false").toOption.get))
            (session, _) <- run(Request.post(URL.decode("/responses/session").toOption.get, zio.http.Body.empty))
            (stream, streamBody) <- run(Request.get(URL.decode("/responses/stream").toOption.get))
          } yield assertTrue(
            png.rawHeader("Content-Type").contains("image/png"),
            svg.rawHeader("Content-Type").contains("image/svg+xml"),
            up.status == Status.Ok,
            upBody == """{"database":"healthy"}""",
            down.status == Status.ServiceUnavailable,
            downBody == """{"database":"unreachable"}""",
            session.status == Status.NoContent,
            session.rawHeader("Mcp-Session-Id").contains("abc"),
            stream.rawHeader("Content-Length").contains("5"),
            streamBody == "hello",
          )).provide(ZLayer.succeed(ResponsesController()))
        }
      ),
      suite("server options")(
        test("Tapir interceptors passed to httpRoutes apply to Okapi routes (CORS)") {
          val options = ZioHttpServerOptions
            .customiseInterceptors[Any]
            .corsInterceptor(sttp.tapir.server.interceptor.cors.CORSInterceptor.default[zio.Task])
            .options
          val request = Request.get(URL.decode("/fx/io").toOption.get).addHeader("Origin", "http://gui.local")
          ZIO
            .scoped(Okapi.httpRoutes[EffectsController](options).runZIO(request))
            .map(response => assertTrue(response.rawHeader("Access-Control-Allow-Origin").contains("*")))
            .provide(ZLayer.succeed(EffectsController()))
        }
      ),
      suite("streams")(
        test("server-sent events and byte streams with a declared media type") {
          val routes = Okapi.httpRoutes[StreamsController]
          def run(path: String) = {
            ZIO.scoped(
              routes.runZIO(Request.get(URL.decode(path).toOption.get)).flatMap(r => r.body.asString.map(r -> _))
            )
          }
          (for {
            (events, eventsBody) <- run("/streams/events")
            (csv, csvBody) <- run("/streams/csv")
          } yield assertTrue(
            events.rawHeader("Content-Type").exists(_.startsWith("text/event-stream")),
            eventsBody == "data: one\nevent: tick\n\ndata: two\n\n",
            csv.rawHeader("Content-Type").exists(_.startsWith("text/csv")),
            csvBody == "a,b\n",
          )).provide(ZLayer.succeed(StreamsController()))
        }
      ),
      suite("WebSocket")(
        test("ping interval per endpoint, typed JSON frames") {
          val outputs = Okapi
            .endpoints[WsOptionsController]
            .flatMap(e => webSocketBody(e.endpoint.output).map(e.endpoint.showPathTemplate(showQueryParam = None) -> _))
            .toMap
          val typed = outputs("/ws/typed").asInstanceOf[sttp.tapir.WebSocketBodyOutput[?, ChatIn, ChatOut, ?, ?]]
          val pings = outputs.view.mapValues(_.autoPing.map(_._1)).toMap
          val encoded = typed.responses.encode(ChatOut("hi"))
          val decoded = typed.requests.decode(sttp.ws.WebSocketFrame.text("""{"text":"yo"}"""))
          assertTrue(
            pings == Map("/ws/typed" -> Some(30.seconds), "/ws/silent" -> None, "/ws/default" -> Some(13.seconds)),
            encoded == sttp.ws.WebSocketFrame.text("""{"echo":"hi"}"""),
            decoded == sttp.tapir.DecodeResult.Value(ChatIn("yo")),
          )
        },
        test("library-typed dependencies become the layer's input") {
          val layer: ZLayer[zio.Random, Nothing, RandomController] = Okapi.autoLayer[Tuple1[RandomController]]
          assertTrue(layer != ZLayer.empty)
        },
        test("the layer's error type is what derivation can fail with (Config.Error for a Config-backed dependency)") {
          val layer = Okapi.controllerLayers[Tuple1[ConfigController]]
          val _ = summon[layer.type <:< ZLayer[Any, zio.Config.Error, ConfigController]]
          val _ = summon[scala.util.NotGiven[layer.type <:< ZLayer[Any, Nothing, ConfigController]]]
          ZIO.service[ConfigController].provide(layer).exit.map(exit => assertTrue(exit.isFailure))
        },
        test("the layer's error type includes a ZLayer.Derive.Scoped lifecycle error") {
          val layer = Okapi.controllerLayers[Tuple1[Hooked]]
          val _ = summon[layer.type <:< ZLayer[Any, String, Hooked]]
          val _ = summon[scala.util.NotGiven[layer.type <:< ZLayer[Any, Nothing, Hooked]]]
          ZIO.scoped(layer.build).exit.map(exit => assertTrue(exit.isFailure))
        },
        test("abstract dependencies become the layer's input") {
          val layer: ZLayer[Clock, Nothing, TimeController] = Okapi.autoLayer[Tuple1[TimeController]]
          get(Okapi.httpRoutes[TimeController], "/time/now")
            .map(r => assertTrue(r == (Status.Ok, """{"text":"42"}""")))
            .provide(layer, ZLayer.succeed(new Clock { def now = 42L }))
        },
      ),
    )
  }
}
