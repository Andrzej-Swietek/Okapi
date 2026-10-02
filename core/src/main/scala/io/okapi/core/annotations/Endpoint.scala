package io.okapi.core
package annotations

import scala.annotation.StaticAnnotation

final class Get(val path: String = "") extends StaticAnnotation
final class Post(val path: String = "") extends StaticAnnotation
final class Put(val path: String = "") extends StaticAnnotation
final class Delete(val path: String = "") extends StaticAnnotation
final class Patch(val path: String = "") extends StaticAnnotation

final class Controller(val basePath: String = "") extends StaticAnnotation
final class Tag(val name: String = "") extends StaticAnnotation

/** Collision-free alias for [[Tag]] — use this when `import zio.*` is in scope (it also exports `zio.Tag`). */
final class ApiTag(val name: String = "") extends StaticAnnotation

final class Query(val name: String = "") extends StaticAnnotation
final class Path(val name: String = "") extends StaticAnnotation
final class Header(val name: String = "") extends StaticAnnotation
final class Cookie(val name: String = "") extends StaticAnnotation

/** Binds an `Authorization: Bearer <token>` header to a `String` parameter and advertises a bearer security scheme. */
final class BearerAuth() extends StaticAnnotation

final class RequestBody() extends StaticAnnotation

final class Produces(val mediaType: String = "application/json") extends StaticAnnotation
final class Consumes(val mediaType: String = "application/json") extends StaticAnnotation

final class Description(val text: String) extends StaticAnnotation
final class Summary(val text: String) extends StaticAnnotation

/** A WebSocket endpoint. The server pings the client every `pingIntervalSeconds` (0 disables pinging). */
final class WebSocket(val path: String = "", val pingIntervalSeconds: Int = 13) extends StaticAnnotation
final class Deprecated() extends StaticAnnotation

/** Overrides the success HTTP status code. Default: 204 for a `Unit` result, otherwise 201 for POST and 200 for the
  * other verbs.
  */
final class Status(val code: Int) extends StaticAnnotation
