package io.okapi.sbt

import zio.test._

import _root_.sbt.internal.util.complete.{ DefaultParsers, Parser }

object OkapiPluginSpec extends ZIOSpecDefault {

  def spec = {
    suite("OkapiPluginSpec")(
      test("a quoted path reaches runMain as one argument, spaces, quotes and backslashes kept") {
        val args = List("spec", """/tmp/my dir/a "b" \c.yaml""", """C:\Users\x y\client.properties""")
        val input = args.map(OkapiPlugin.quote).mkString(" ", " ", "")
        val parsed = Parser.parse(input, DefaultParsers.spaceDelimited("<arg>"))
        assertTrue(parsed == Right(args))
      }
    )
  }
}
