package io.okapi.exampleApp

import zio.*
import io.okapi.core.http.ApiError

final class NotificationService {
  def notify(message: String): Unit =
    println(s"[NOTIFICATION] $message")
}

final class BookService(repo: BookRepository, notifier: NotificationService) {

  def listBooks(genre: Option[String], limit: Int): IO[ApiError, List[Book]] =
    ZIO.succeed(repo.findAll(genre, limit))

  def getBook(id: Int): IO[ApiError, Book] =
    ZIO.fromOption(repo.findById(id))
      .orElseFail(ApiError.NotFound(s"Book $id not found"))

  def createBook(req: CreateBookRequest): IO[ApiError, Book] =
    ZIO.succeed {
      val book = repo.save(req)
      notifier.notify(s"Book created: ${book.title}")
      book
    }

  def updateBook(id: Int, req: CreateBookRequest): IO[ApiError, Book] =
    ZIO.fromOption(repo.update(id, req))
      .orElseFail(ApiError.NotFound(s"Book $id not found"))

  def deleteBook(id: Int): IO[ApiError, Unit] =
    if repo.delete(id) then ZIO.unit
    else ZIO.fail(ApiError.NotFound(s"Book $id not found"))

  def getStats: IO[ApiError, BookStats] =
    ZIO.succeed(BookStats(total = repo.count, genres = repo.genres))

  def popularByGenreYear(genre: String, year: Int, limit: Int): IO[ApiError, List[Book]] =
    ZIO.succeed(repo.findAll(Some(genre)).filter(_.year == year).take(limit))

  def exportCsv: IO[ApiError, Array[Byte]] =
    listBooks(None, Int.MaxValue).map { books =>
      val header = "id,title,author,genre,year\n"
      val rows   = books.map(b => s"${b.id},${b.title},${b.author},${b.genre},${b.year}").mkString("\n")
      (header + rows).getBytes("UTF-8").nn
    }
}

final class AuthService(userRepo: UserRepository, notifier: NotificationService) {

  def listUsers: IO[ApiError, List[User]] =
    ZIO.succeed(userRepo.findAll)

  def getUser(id: Int): IO[ApiError, User] =
    ZIO.fromOption(userRepo.findById(id))
      .orElseFail(ApiError.NotFound(s"User $id not found"))

  def createUser(req: CreateUserRequest): IO[ApiError, User] =
    userRepo.findByUsername(req.username) match {
      case Some(_) => ZIO.fail(ApiError.BadRequest(s"Username '${req.username}' already taken"))
      case None    =>
        ZIO.succeed {
          val user = userRepo.save(req)
          notifier.notify(s"User registered: ${user.username}")
          user
        }
    }

  def getPreferences(userId: Int): IO[ApiError, UserPreferences] =
    getUser(userId).as(UserPreferences(userId, theme = "dark", language = "en"))
}
