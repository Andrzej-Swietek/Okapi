package io.okapi.exampleApp

import zio.json.JsonCodec

final case class Book(
  id: Int,
  title: String,
  author: String,
  genre: String,
  year: Int,
) derives JsonCodec

final case class CreateBookRequest(
  title: String,
  author: String,
  genre: String,
  year: Int,
) derives JsonCodec

final case class BookReview(
  bookId: Int,
  reviewer: String,
  rating: Int,
  comment: String,
) derives JsonCodec

final case class BookCoverDto(
  bookId: Int,
  coverTitle: String,
  altText: String,
) derives JsonCodec

final case class BookStats(
  total: Int,
  genres: List[String],
) derives JsonCodec

// ─── Users ────────────────────────────────────────────────────────────────────

final case class User(
  id: Int,
  username: String,
  email: String,
) derives JsonCodec

final case class CreateUserRequest(
  username: String,
  email: String,
) derives JsonCodec

final case class UserPreferences(
  userId: Int,
  theme: String,
  language: String,
) derives JsonCodec

// ─── Admin ────────────────────────────────────────────────────────────────────

final case class HealthDto(
  status: String,
  uptime: Long,
  version: String,
) derives JsonCodec
