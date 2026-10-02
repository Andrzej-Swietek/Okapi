package io.okapi.exampleclient.api

import io.okapi.exampleclient.api.models.BookStats

/** The operations of `Admin`. */
trait AdminRoutes[F[_]] {

  /** Export all books as CSV (binary download) */
  def exportCsv(): F[Array[Byte]]

  /** Health check */
  def health(): F[String]

  /** Get book statistics (JSON) */
  def stats(): F[BookStats]
}
