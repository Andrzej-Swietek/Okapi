package io.okapi.exampleApp

import zio.IO
import io.okapi.core.annotations.*
import io.okapi.core.http.ApiError

@Controller("/api/books")
@Tag("Books")
final class BookController(bookService: BookService) {

  @Get("")
  @Summary("List books")
  def listBooks(
    @Query("genre") genre: Option[String],
    @Query("limit") limit: Option[Int],
  ): IO[ApiError, List[Book]] =
    bookService.listBooks(genre, limit.getOrElse(50))

  @Post("")
  @Summary("Create a new book")
  @Consumes("application/json")
  def createBook(
    @RequestBody body: CreateBookRequest,
  ): IO[ApiError, Book] =
    bookService.createBook(body)

  @Get("/{id}")
  @Summary("Get book by ID")
  def getBook(@Path("id") id: Int): IO[ApiError, Book] =
    bookService.getBook(id)

  @Put("/{id}")
  @Summary("Update book")
  @Consumes("application/json")
  def updateBook(
    @Path("id") id: Int,
    @RequestBody body: CreateBookRequest,
  ): IO[ApiError, Book] =
    bookService.updateBook(id, body)

  @Delete("/{id}")
  @Summary("Delete book")
  def deleteBook(@Path("id") id: Int): IO[ApiError, Unit] =
    bookService.deleteBook(id)

  @Get("/{id}/reviews")
  @Summary("Get reviews for a book")
  def getReviews(
    @Path("id") id: Int,
    @Query("limit") limit: Option[Int],
  ): IO[ApiError, List[BookReview]] =
    bookService.getBook(id).as(
      List(
        BookReview(id, "alice", 5, "Excellent!"),
        BookReview(id, "bob",   4, "Very good"),
      ).take(limit.getOrElse(10))
    )

  @Get("/stats")
  @Summary("Get overall book statistics")
  def stats: IO[ApiError, BookStats] =
    bookService.getStats
}
