package io.okapi.exampleApp

import zio.IO
import zio.stream.ZStream
import io.okapi.core.WsPipe
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/ws")
@Tag("WebSocket")
final class WsController(bookService: BookService) {

  @WebSocket("/echo")
  @Summary("Echo WebSocket — mirrors every text message back")
  def echo: WsPipe[String, String] =
    _.map(msg => s"echo: $msg")

  @WebSocket("/chat/{room}")
  @Summary("Chat room — annotates messages with room and user")
  def chat(
    @Path("room")  room: String,
    @Query("user") user: Option[String],
  ): WsPipe[String, String] =
    _.map(msg => s"[$room] ${user.getOrElse("anon")}: $msg")

  @WebSocket("/book-updates")
  @Summary("Stream live book-count updates as text")
  def bookUpdates: IO[ApiError, WsPipe[String, String]] =
    bookService.getStats.map { initialStats =>
      stream =>
        ZStream.succeed(s"connected books=${initialStats.total}") ++
        stream.map(cmd => if cmd == "stats" then s"books=${initialStats.total}" else s"unknown: $cmd")
    }
}
