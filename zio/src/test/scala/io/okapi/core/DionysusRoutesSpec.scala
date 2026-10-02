package io.okapi.core

import zio.{ Chunk, IO, Ref, ZIO, ZLayer }
import zio.http.{ Body, Form, FormField, MediaType as HttpMediaType, Request, Status, URL }
import zio.test.*

import io.okapi.core.annotations.{
  Consumes,
  Controller,
  Get,
  Path,
  Post,
  Produces,
  Query,
  RequestBody,
  Status as HttpStatus,
}
import io.okapi.core.http.{ ApiError, ApiResponse }
import io.okapi.core.json.JsoniterCodec
import scala.language.unsafeNulls // Tapir's derived MultipartCodec is not explicit-nulls safe
import sttp.model.{ MediaType, Part }
import sttp.tapir.generic.auto.*

/** The routes Project Dionysus' device-manager serves raw (firmware, storage and device-image uploads, image download
  * with a per-file Content-Type), written as one Okapi controller and called over HTTP.
  */
object DionysusRoutesSpec extends ZIOSpecDefault {

  final case class FirmwareUploadForm(
    id: String,
    deviceId: String,
    version: String,
    checksum: String,
    releaseNotes: Option[String],
    file: Part[Array[Byte]],
  )

  final case class FileForm(file: Part[Array[Byte]])

  final case class FirmwareResponse(id: String, deviceId: String, filePath: String, releaseNotes: Option[String])
    derives JsoniterCodec

  final class Store(val files: Ref[Map[String, Array[Byte]]])

  @Controller("/api")
  final class DeviceManagerController(store: Store) {

    @Post("/firmwares/upload")
    @Consumes("multipart/form-data")
    @HttpStatus(201)
    def uploadFirmware(@RequestBody form: FirmwareUploadForm): IO[ApiError, FirmwareResponse] = {
      val path = s"${form.deviceId}/${form.id}/${form.file.fileName.getOrElse(form.id + ".bin")}"
      store
        .files
        .update(_ + (path -> form.file.body))
        .as(FirmwareResponse(form.id, form.deviceId, path, form.releaseNotes))
    }

    @Post("/storage/upload")
    @Consumes("multipart/form-data")
    @HttpStatus(201)
    @Produces("text/plain")
    def storageUpload(@Query("path") path: String, @RequestBody form: FileForm): IO[ApiError, String] =
      store.files.update(_ + (path -> form.file.body)).as("Uploaded successfully")

    @Post("/devices/{id}/meta/image")
    @Consumes("multipart/form-data")
    @HttpStatus(200)
    def uploadImage(@Path("id") id: String, @RequestBody form: FileForm): IO[ApiError, Unit] = {
      val name = form.file.fileName.getOrElse("image")
      store.files.update(_ + (s"devices/$id/$name" -> form.file.body)).unit
    }

    @Get("/devices/{id}/meta/image")
    def image(@Path("id") id: String): IO[ApiError, ApiResponse[Array[Byte]]] = {
      store.files.get.flatMap { files =>
        files.collectFirst { case (key, bytes) if key.startsWith(s"devices/$id/") => key -> bytes } match {
          case Some((key, bytes)) => ZIO.succeed(ApiResponse(bytes).withContentType(mediaTypeOf(key)))
          case None => ZIO.fail(ApiError.NotFound(s"No image for device $id"))
        }
      }
    }

    private def mediaTypeOf(key: String): MediaType = {
      key.split('.').nn.lastOption.map(_.nn.toLowerCase.nn) match {
        case Some("png") => MediaType.ImagePng
        case Some("gif") => MediaType.ImageGif
        case Some("svg") => MediaType.unsafeParse("image/svg+xml")
        case _ => MediaType.ImageJpeg
      }
    }
  }

  private def multipart(path: String, fields: FormField*): Request = {
    Request.post(
      URL.decode(path).toOption.get,
      Body.fromMultipartForm(Form(fields*), zio.http.Boundary("okapiboundary")),
    )
  }

  private def file(name: String, fileName: String, bytes: String) = {
    FormField.binaryField(
      name,
      Chunk.fromArray(bytes.getBytes.nn),
      HttpMediaType.application.`octet-stream`,
      filename = Some(fileName),
    )
  }

  override def spec = {
    test("firmware, storage and image uploads, and the image with its per-file Content-Type") {
      val routes = Okapi.httpRoutes[DeviceManagerController]
      def run(request: Request) = ZIO.scoped(routes.runZIO(request).flatMap(r => r.body.asString.map(r -> _)))
      (for {
        (firmware, firmwareBody) <- run(
          multipart(
            "/api/firmwares/upload",
            FormField.textField("id", "fw-1"),
            FormField.textField("deviceId", "dev-9"),
            FormField.textField("version", "1.2.0"),
            FormField.textField("checksum", "abc"),
            file("file", "fw.bin", "BINARY"),
          )
        )
        (storage, storageBody) <- run(
          multipart("/api/storage/upload?path=docs/readme.txt", file("file", "readme.txt", "hello"))
        )
        (upload, _) <- run(multipart("/api/devices/dev-9/meta/image", file("file", "photo.png", "PNG")))
        (image, imageBody) <- run(Request.get(URL.decode("/api/devices/dev-9/meta/image").toOption.get))
        (missing, _) <- run(Request.get(URL.decode("/api/devices/none/meta/image").toOption.get))
      } yield assertTrue(
        firmware.status == Status.Created,
        firmwareBody == """{"id":"fw-1","deviceId":"dev-9","filePath":"dev-9/fw-1/fw.bin"}""",
        storage.status == Status.Created,
        storageBody == "Uploaded successfully",
        storage.rawHeader("Content-Type").exists(_.startsWith("text/plain")),
        upload.status == Status.Ok,
        image.status == Status.Ok,
        image.rawHeader("Content-Type").contains("image/png"),
        imageBody == "PNG",
        missing.status == Status.NotFound,
      )).provide(
        ZLayer.fromZIO(Ref.make(Map.empty[String, Array[Byte]]).map(Store(_))),
        ZLayer.derive[DeviceManagerController],
      )
    }
  }
}
