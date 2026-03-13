package com.okapi.exampleApp

import zio.json.JsonCodec

import com.okapi.core.annotations.{ Controller, Get, Query, Summary, Tag }

final case class AdminStatusDto(area: String, enabled: Boolean) derives JsonCodec

@Controller("/api/admin")
@Tag("Admin")
final class AdminController {

  @Get("/ping")
  @Summary("Admin ping endpoint")
  def ping(
    @Query("source") source: Option[String]
  ): String =
    s"pong:${source.getOrElse("admin")}"

  @Get("/status")
  def status: AdminStatusDto =
    AdminStatusDto(area = "admin", enabled = true)
}
