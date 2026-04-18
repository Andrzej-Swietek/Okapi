package io.okapi.exampleApp

import zio.{ IO, ZIO }
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/covers")
@Tag("Covers")
final class CoverController(coverRepo: CoverRepository, bookService: BookService) {

  @Post("/{bookId}")
  @Summary("Upload a book cover image (raw binary, title/altText as query params)")
  @Consumes("application/octet-stream")
  def uploadCover(
    @Path("bookId")       bookId: Int,
    @Query("title")       title: Option[String],
    @Query("altText")     altText: Option[String],
    @RequestBody          bytes: Array[Byte],
  ): IO[ApiError, BookCoverDto] =
    bookService.getBook(bookId).flatMap { _ =>
      ZIO.succeed {
        coverRepo.save(bookId, bytes, s"cover_$bookId.bin")
        BookCoverDto(bookId, title.getOrElse("Cover"), altText.getOrElse(""))
      }
    }

  @Get("/{bookId}")
  @Summary("Download book cover as raw bytes")
  @Produces("application/octet-stream")
  def downloadCover(@Path("bookId") bookId: Int): IO[ApiError, Array[Byte]] =
    ZIO.fromOption(coverRepo.find(bookId))
      .orElseFail(ApiError.NotFound(s"No cover found for book $bookId"))
      .map(_._1)

  @Delete("/{bookId}")
  @Summary("Delete book cover")
  def deleteCover(@Path("bookId") bookId: Int): IO[ApiError, Unit] =
    if coverRepo.delete(bookId) then ZIO.unit
    else ZIO.fail(ApiError.NotFound(s"No cover found for book $bookId"))
}
