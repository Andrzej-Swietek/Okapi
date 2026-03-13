package com.okapi.core
package annotations

import scala.annotation.{ experimental, StaticAnnotation }

final class Get(val path: String = "") extends StaticAnnotation
final class Post(val path: String = "") extends StaticAnnotation
final class Put(val path: String = "") extends StaticAnnotation
final class Delete(val path: String = "") extends StaticAnnotation
final class Patch(val path: String = "") extends StaticAnnotation

final class Controller(val basePath: String = "") extends StaticAnnotation
final class Tag(val name: String = "") extends StaticAnnotation

final class Query(val name: String = "") extends StaticAnnotation
final class Path(val name: String = "") extends StaticAnnotation
final class Header(val name: String = "") extends StaticAnnotation
final class Cookie(val name: String = "") extends StaticAnnotation

final class RequestBody() extends StaticAnnotation

final class Produces(val mediaType: String = "application/json") extends StaticAnnotation
final class Consumes(val mediaType: String = "application/json") extends StaticAnnotation

final class Description(val text: String) extends StaticAnnotation
final class Summary(val text: String) extends StaticAnnotation
