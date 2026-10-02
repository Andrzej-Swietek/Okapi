package io.okapi.exampleApp

import zio.IO

import java.lang.System as JSystem

import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/admin")
@Tag("Admin")
final class AdminController(bookService: BookService) {

  private val startedAt = JSystem.currentTimeMillis()

  @Get("/health")
  @Summary("Health check")
  @Produces("text/plain")
  def health: String =
    s"OK uptime=${JSystem.currentTimeMillis() - startedAt}ms"

  @Get("/stats")
  @Summary("Get book statistics (JSON)")
  def stats: IO[ApiError, BookStats] =
    bookService.getStats

  @Get("/export")
  @Summary("Export all books as CSV (binary download)")
  @Produces("application/octet-stream")
  def exportCsv: IO[ApiError, Array[Byte]] =
    bookService.exportCsv
}
