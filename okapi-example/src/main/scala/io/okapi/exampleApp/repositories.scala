package io.okapi.exampleApp

import scala.collection.mutable

final class BookRepository {
  private val store   = mutable.HashMap.empty[Int, Book]
  private var counter = 1

  def findAll(genre: Option[String] = None, limit: Int = 50): List[Book] =
    store.values.filter(b => genre.forall(_ == b.genre)).take(limit).toList

  def findById(id: Int): Option[Book] = store.get(id)

  def save(req: CreateBookRequest): Book = {
    val book = Book(counter, req.title, req.author, req.genre, req.year)
    store(counter) = book
    counter += 1
    book
  }

  def update(id: Int, req: CreateBookRequest): Option[Book] =
    if store.contains(id) then {
      val book = Book(id, req.title, req.author, req.genre, req.year)
      store(id) = book
      Some(book)
    } else None

  def delete(id: Int): Boolean = store.remove(id).isDefined

  def genres: List[String] = store.values.map(_.genre).toList.distinct.sorted

  def count: Int = store.size
}

final class UserRepository {
  private val store   = mutable.HashMap.empty[Int, User]
  private var counter = 1

  def findAll: List[User] = store.values.toList

  def findById(id: Int): Option[User] = store.get(id)

  def findByUsername(username: String): Option[User] =
    store.values.find(_.username == username)

  def save(req: CreateUserRequest): User = {
    val user = User(counter, req.username, req.email)
    store(counter) = user
    counter += 1
    user
  }

  def count: Int = store.size
}

final class CoverRepository {
  private val store = mutable.HashMap.empty[Int, (Array[Byte], String)]

  def save(bookId: Int, data: Array[Byte], filename: String): Unit =
    store(bookId) = (data, filename)

  def find(bookId: Int): Option[(Array[Byte], String)] = store.get(bookId)

  def delete(bookId: Int): Boolean = store.remove(bookId).isDefined
}
