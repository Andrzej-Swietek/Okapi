package io.okapi.core
package annotations

import scala.annotation.StaticAnnotation

/** Routes the method to `GET` at `path`, appended to the [[Controller]] base path. A `{name}` segment captures the
  * [[Path]] parameter `name`; a [[Path]] parameter the path does not mention is appended as a trailing capture.
  */
final class Get(val path: String = "") extends StaticAnnotation

/** Like [[Get]], for `POST`. */
final class Post(val path: String = "") extends StaticAnnotation

/** Like [[Get]], for `PUT`. */
final class Put(val path: String = "") extends StaticAnnotation

/** Like [[Get]], for `DELETE`. */
final class Delete(val path: String = "") extends StaticAnnotation

/** Like [[Get]], for `PATCH`. */
final class Patch(val path: String = "") extends StaticAnnotation

/** The path prefix of every route of the annotated class. */
final class Controller(val basePath: String = "") extends StaticAnnotation

/** The OpenAPI tag of every endpoint of the annotated class; without a name, the class's simple name. */
final class Tag(val name: String = "") extends StaticAnnotation

/** Alias for [[Tag]], for scopes where another `Tag` is imported; wins over [[Tag]] when both are present. */
final class ApiTag(val name: String = "") extends StaticAnnotation

/** Reads the parameter from query parameter `name` (default: the parameter's name), optional when the parameter has a
  * default value. A parameter without an HTTP annotation is read as `@Query`, with a compile warning.
  */
final class Query(val name: String = "") extends StaticAnnotation

/** Binds the parameter to the path capture `{name}` (default: the parameter's name). Required: a default value is
  * ignored with a compile warning.
  */
final class Path(val name: String = "") extends StaticAnnotation

/** Reads the parameter from request header `name` (default: the parameter's name), optional when the parameter has a
  * default value.
  */
final class Header(val name: String = "") extends StaticAnnotation

/** Reads the parameter from cookie `name` (default: the parameter's name), optional when the parameter has a default
  * value.
  */
final class Cookie(val name: String = "") extends StaticAnnotation

/** Binds the token of an `Authorization: Bearer <token>` header to the parameter and advertises a bearer security
  * scheme. Required: a default value is ignored with a compile warning.
  */
final class BearerAuth() extends StaticAnnotation

/** Binds the request body to the parameter, decoded by its type and [[Consumes]]. At most one per method. */
final class RequestBody() extends StaticAnnotation

/** The response body's media type, validated at compile time. Absent or without an argument: JSON for a typed body,
  * `text/plain` for `String`, `application/octet-stream` for `Array[Byte]`. A typed body supports JSON
  * (`application/json`, `*+json`) and `application/x-www-form-urlencoded`.
  */
final class Produces(val mediaType: String = "application/json") extends StaticAnnotation

/** The request body's media type, with the defaults of [[Produces]]. A typed body also supports `multipart/form-data`.
  */
final class Consumes(val mediaType: String = "application/json") extends StaticAnnotation

/** The endpoint's OpenAPI description. */
final class Description(val text: String) extends StaticAnnotation

/** The endpoint's OpenAPI summary. */
final class Summary(val text: String) extends StaticAnnotation

/** A WebSocket endpoint. The server pings the client every `pingIntervalSeconds` (0 or less disables pinging). */
final class WebSocket(val path: String = "", val pingIntervalSeconds: Int = 13) extends StaticAnnotation

/** Marks the endpoint deprecated. */
final class Deprecated() extends StaticAnnotation

/** Overrides the success HTTP status code. Default: 204 for a `Unit` result, otherwise 201 for POST and 200 for the
  * other verbs.
  */
final class Status(val code: Int) extends StaticAnnotation
