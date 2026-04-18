package io.okapi.exampleApp

import zio.IO
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/explore")
@Tag("Explore")
final class ExploreController(bookService: BookService) {

  @Get("/{genre}/{year}/popular")
  @Summary("Popular books by genre and year (plain text)")
  @Produces("text/plain")
  def popularByGenreYear(
    @Path("genre") genre: String,
    @Path("year") year: Int,
    @Query("limit") limit: Option[Int],
  ): IO[ApiError, String] =
    bookService.popularByGenreYear(genre, year, limit.getOrElse(10)).map { books =>
      if books.isEmpty then s"No books found for genre='$genre' year=$year"
      else books.map(b => s"${b.title} (${b.author}, ${b.year})").mkString("\n")
    }

  @Get("/genres")
  @Summary("List all available genres (plain text)")
  @Produces("text/plain")
  def genres: IO[ApiError, String] =
    bookService.getStats.map(_.genres.mkString(", "))
}
