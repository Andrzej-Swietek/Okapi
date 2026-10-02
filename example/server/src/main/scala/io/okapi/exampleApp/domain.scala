package io.okapi.exampleApp

import io.okapi.core.json.JsoniterCodec

final case class Book(
  id: Int,
  title: String,
  author: String,
  genre: String,
  year: Int,
) derives JsoniterCodec

final case class CreateBookRequest(
  title: String,
  author: String,
  genre: String,
  year: Int,
) derives JsoniterCodec

final case class BookReview(
  bookId: Int,
  reviewer: String,
  rating: Int,
  comment: String,
) derives JsoniterCodec

final case class BookCoverDto(
  bookId: Int,
  coverTitle: String,
  altText: String,
) derives JsoniterCodec

final case class BookStats(
  total: Int,
  genres: List[String],
) derives JsoniterCodec

final case class User(
  id: Int,
  username: String,
  email: String,
) derives JsoniterCodec

final case class CreateUserRequest(
  username: String,
  email: String,
) derives JsoniterCodec

final case class UserPreferences(
  userId: Int,
  theme: String,
  language: String,
) derives JsoniterCodec

final case class HealthDto(
  status: String,
  uptime: Long,
  version: String,
) derives JsoniterCodec
