package io.okapi.exampleclient.api

import io.okapi.exampleclient.api.models.{ Book, BookReview, BookStats, CreateBookRequest }

/** The operations of `Books`. */
trait BooksRoutes[F[_]] {

  /** List books */
  def listBooks(genre: Option[String] = None, limit: Option[Int] = None): F[List[Book]]

  /** Create a new book */
  def createBook(createBookRequest: CreateBookRequest): F[Book]

  /** Get overall book statistics */
  def stats(): F[BookStats]

  /** Get book by ID */
  def getBook(id: Int): F[Book]

  /** Update book */
  def updateBook(id: Int, createBookRequest: CreateBookRequest): F[Book]

  /** Delete book */
  def deleteBook(id: Int): F[Unit]

  /** Get reviews for a book */
  def getReviews(id: Int, limit: Option[Int] = None): F[List[BookReview]]
}
