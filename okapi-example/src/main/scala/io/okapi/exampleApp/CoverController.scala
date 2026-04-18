package io.okapi.exampleApp

import zio.{ IO, ZIO }
import zio.stream.ZStream
import io.okapi.core.annotations.*
import io.okapi.core.http.{ ApiError, FileResponse }

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

  @Post("/{bookId}/stream")
  @Summary("Upload a book cover image (streaming binary body)")
  @Consumes("application/octet-stream")
  def uploadCoverStreaming(
    @Path("bookId")   bookId: Int,
    @Query("title")   title: Option[String],
    @RequestBody      stream: ZStream[Any, Throwable, Byte],
  ): IO[ApiError, BookCoverDto] =
    bookService.getBook(bookId).flatMap { _ =>
      stream.runCollect.mapError(_ => ApiError.Internal("Failed to read stream")).flatMap { chunk =>
        ZIO.succeed {
          coverRepo.save(bookId, chunk.toArray, s"cover_$bookId.bin")
          BookCoverDto(bookId, title.getOrElse("Cover"), "")
        }
      }
    }

  @Get("/{bookId}")
  @Summary("Download book cover with Content-Disposition header")
  def downloadCover(@Path("bookId") bookId: Int): IO[ApiError, FileResponse] =
    ZIO.fromOption(coverRepo.find(bookId))
      .orElseFail(ApiError.NotFound(s"No cover found for book $bookId"))
      .map { case (bytes, filename) => FileResponse(bytes, filename) }

  @Delete("/{bookId}")
  @Summary("Delete book cover")
  def deleteCover(@Path("bookId") bookId: Int): IO[ApiError, Unit] =
    if coverRepo.delete(bookId) then ZIO.unit
    else ZIO.fail(ApiError.NotFound(s"No cover found for book $bookId"))
}
