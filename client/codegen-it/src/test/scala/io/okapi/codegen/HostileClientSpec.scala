package io.okapi.codegen

import zio.test.*

import java.util.UUID

import scala.collection.mutable
// the specs drive JDK and sttp APIs typed with nulls
import scala.language.unsafeNulls
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromString, writeToString }
import hostile.client.impl.SttpHostile
import hostile.client.models.*
import sttp.client4.*
import sttp.client4.testing.SyncBackendStub

/** The client of `hostile.yaml`, whose names collide with Scala, sttp and the generated code; its fs2 and zio variants
  * are compiled alongside it.
  */
object HostileClientSpec extends ZIOSpecDefault {

  private def client(reply: String): (SttpHostile[sttp.shared.Identity], mutable.Buffer[String]) = {
    val sent = mutable.Buffer.empty[String]
    val backend = SyncBackendStub
      .whenRequestMatches { r =>
        val body = r.body match {
          case b: ByteArrayBody => new String(b.b)
          case _ => ""
        }
        val headers = r.headers.filter(h => Set("2fa", "X-Trace", "X-Ids", "X-Tags").contains(h.name)).mkString(" ")
        sent += s"${r.method} ${r.uri} $headers :: $body"
        true
      }
      .thenRespondAdjust(reply)
    (SttpHostile(backend, uri"http://h"), sent)
  }

  private val trace = UUID.fromString("00000000-0000-0000-0000-000000000001")

  def spec = {
    suite("HostileClientSpec")(
      test("a model named like a Scala, sttp or generated type is suffixed with Model and still decodes") {
        val reply = """[{"class":"c","x-y":1,"3d":true,"header":{"name":"n"},"when":"2026-01-01T00:00:00Z"}]"""
        val (api, _) = client(reply)
        val items = api.userAccounts.listItems("u", trace)
        assertTrue(
          items == List(
            RequestModel(
              "c",
              1,
              Some(true),
              Some(HeaderModel(Some("n"))),
              Some(
                java.time.Instant.parse(
                  "2026-01-01T00:00:00Z"
                )
              ),
            )
          )
        )
      },
      test("path values are escaped, enums sent by value, lists repeated, odd header names kept") {
        val (api, sent) = client("[]")
        val _ = api.userAccounts.listItems("u 1/2", trace, Some(Kind.AB2), List(Kind.AB, Kind._1st), Some(true))
        assertTrue(
          sent.toList == List(
            "GET http://h/api/user-accounts/u%201%2F2/items?type=A+B&kinds=a-b&kinds=1st " +
              "X-Trace: 00000000-0000-0000-0000-000000000001 2fa: true :: "
          )
        )
      },
      test("a path literal with $ and a space is encoded") {
        val (api, sent) = client("""{"task":{"done":true}}""")
        val stream = api.trees.weird(7)
        assertTrue(
          stream == StreamModel(Some(TaskModel(Some(true)))),
          sent.toList == List("GET http://h/api/trees/7/$weird%20path/x  :: "),
        )
      },
      test("discriminator values come from the mapping, in both directions") {
        val (api, sent) = client("""{"items":["a"]}""")
        val _ = api.trees.draw(Circle(1.5))
        val _ = api.trees.draw(Square)
        assertTrue(
          sent.toList == List(
            """POST http://h/api/shapes  :: {"kind":"circle","r":1.5}""",
            """POST http://h/api/shapes  :: {"kind":"square"}""",
          ),
          readFromString[Shape]("""{"kind":"circle","r":2}""") == Circle(2),
          readFromString[Shape]("""{"kind":"square"}""") == Square,
        )
      },
      test("a field-less case used as a type of its own is a case class") {
        val (api, sent) = client("""{"kind":"point"}""")
        val origin = api.trees.origin()
        val _ = api.trees.draw(Point())
        assertTrue(origin == Point(), sent.last == """POST http://h/api/shapes  :: {"kind":"point"}""")
      },
      test("allOf cases of a discriminated oneOf are records with the merged properties") {
        val (api, sent) = client("""{"petType":"Cat","name":"Tom","owner":{"name":"Ann"}}""")
        val adopted = api.pets.adopt(Dog("Rex", true))
        val (cards, _) = client("""{"petType":"Dog","name":"Rex","score":3}""")
        assertTrue(
          adopted == Cat("Tom", Some(HeaderModel(Some("Ann")))),
          sent.toList == List("""POST http://h/api/pets  :: {"petType":"Dog","name":"Rex","bark":true}"""),
          cards.pets.card() == CardResponse("Dog", "Rex", 3),
        )
      },
      test("a discriminated oneOf without object cases is raw JSON") {
        val (api, _) = client("""{"k":"a-b"}""")
        assertTrue(api.pets.odd() == RawJson("""{"k":"a-b"}"""))
      },
      test("a sealed trait whose case contains itself encodes and decodes") {
        val (api, sent) = client("""[{"name":"x","children":[{"name":"y"}]}]""")
        val folders = api.entries.entries(Folder("root", List(Folder("a"))))
        assertTrue(
          folders == List(Folder("x", List(Folder("y")))),
          sent.toList == List(
            """PUT http://h/api/entries  :: {"kind":"Folder","name":"root","children":[{"name":"a"}]}"""
          ),
          readFromString[Entry]("""{"kind":"File","name":"f"}""") == File("f"),
        )
      },
      test("captures inside a path segment, list headers joined by commas, a tag starting with a digit") {
        val (api, sent) = client("")
        val _ = api.files.file("a b", List("1", "2"), List("x", "y"))
        val _ = api.files.file("n", List("1"))
        val _ = api.files.predict(5)
        val _ = api._3dModels.report("csv", "a", "b")
        assertTrue(
          sent.toList == List(
            "GET http://h/api/files/a%20b.json X-Ids: 1,2 X-Tags: x,y :: ",
            "GET http://h/api/files/n.json X-Ids: 1 :: ",
            "POST http://h/api/models/5:predict  :: ",
            "GET http://h/api/report.csv/ab  :: ",
          )
        )
      },
      test("colliding field names get distinct parameters; a required empty list is written") {
        val (api, sent) =
          client("""{"user_id":"u","userId":"v","@type":"t","type":"T","Name":"N","name":"n","labels":[]}""")
        val back = api.files.collide(Collide("u", "v", labels = Nil))
        assertTrue(
          back == Collide("u", "v", Some("t"), Some("T"), Some("N"), Some("n"), Nil),
          sent.toList == List("""POST http://h/api/collide  :: {"user_id":"u","userId":"v","labels":[]}"""),
          writeToString(Collide("u", "v", `type` = Some("t"), type2 = Some("T"), labels = List("l")))
            .contains(""""@type":"t","type":"T""""),
        )
      },
    )
  }
}
