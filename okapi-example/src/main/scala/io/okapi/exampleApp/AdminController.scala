package io.okapi.exampleApp

import zio.IO
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/admin")
@Tag("Admin")
final class AdminController(bookService: BookService) {

  @Get("/health")
  @Summary("Health check")
  @Produces("text/plain")
  def health: String =
    s"OK uptime=${java.lang.System.currentTimeMillis()}ms"

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
