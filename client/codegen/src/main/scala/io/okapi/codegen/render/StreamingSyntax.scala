package io.okapi.codegen.render

import io.okapi.codegen.Streaming

/** What the generated sources write for each [[Streaming]] mode. */
private[codegen] object StreamingSyntax {

  private val Events = "sttp.model.sse.ServerSentEvent"

  extension (streaming: Streaming) {

    /** The type of a streamed binary body in the API traits. */
    def binary: String = streaming match {
      case Streaming.Fs2 => "Stream[F, Byte]"
      case Streaming.Zio => "ZStream[Any, Throwable, Byte]"
      case Streaming.Disabled => "Array[Byte]"
    }

    /** The type of a `text/event-stream` response in the API traits. */
    def events: String = streaming match {
      case Streaming.Fs2 => "Stream[F, ServerSentEvent]"
      case Streaming.Zio => "ZStream[Any, Throwable, ServerSentEvent]"
      case Streaming.Disabled => "List[ServerSentEvent]"
    }

    def binaryImports: List[String] = streaming match {
      case Streaming.Fs2 => List("fs2.Stream")
      case Streaming.Zio => List("zio.stream.ZStream")
      case Streaming.Disabled => Nil
    }

    def eventImports: List[String] = Events :: binaryImports

    /** The type parameters of the implementation classes: the zio implementation is in `Task`. */
    def typeParams: String = if (streaming == Streaming.Zio) "" else "[F[_]]"

    def typeArgs: String = if (streaming == Streaming.Zio) "" else "[F]"

    /** The effect of the implementation. */
    def effect: String = if (streaming == Streaming.Zio) "Task" else "F"

    def effectImports: List[String] = if (streaming == Streaming.Zio) List("zio.Task") else Nil

    def backend: String = streaming match {
      case Streaming.Fs2 => "StreamBackend[F, Fs2Streams[F]]"
      case Streaming.Zio => "StreamBackend[Task, ZioStreams]"
      case Streaming.Disabled => "Backend[F]"
    }

    def backendImports: List[String] = streaming match {
      case Streaming.Fs2 => List("sttp.client4.StreamBackend", "sttp.capabilities.fs2.Fs2Streams")
      case Streaming.Zio => List("sttp.client4.StreamBackend", "sttp.capabilities.zio.ZioStreams", "zio.Task")
      case Streaming.Disabled => List("sttp.client4.Backend")
    }

    /** The capabilities of the requests the transport sends. */
    def capabilities: String = streaming match {
      case Streaming.Fs2 => "Fs2Streams[F] & Effect[F]"
      case Streaming.Zio => "ZioStreams & Effect[Task]"
      case Streaming.Disabled => "Any"
    }

    def streamsType: String = streaming match {
      case Streaming.Fs2 => "Fs2Streams[F]"
      case _ => "ZioStreams"
    }

    def streamsValue: String = streaming match {
      case Streaming.Fs2 => "Fs2Streams[F]"
      case _ => "ZioStreams"
    }

    /** The pipe parsing a byte stream into server-sent events. */
    def eventParser: String = streaming match {
      case Streaming.Fs2 => "Fs2ServerSentEvents.parse[F]"
      case _ => "ZioServerSentEvents.parse"
    }

    def eventParserImport: String = streaming match {
      case Streaming.Fs2 => "sttp.client4.impl.fs2.Fs2ServerSentEvents"
      case _ => "sttp.client4.impl.zio.ZioServerSentEvents"
    }

    /** The sttp client4 module of the streams, as an sbt dependency. */
    def dependency(version: String): Option[String] = streaming match {
      case Streaming.Fs2 => Some(s""""com.softwaremill.sttp.client4" %% "fs2" % "$version"""")
      case Streaming.Zio => Some(s""""com.softwaremill.sttp.client4" %% "zio" % "$version"""")
      case Streaming.Disabled => None
    }
  }
}
