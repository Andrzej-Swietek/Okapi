package io.okapi.core.macros.model

/** A route's URL template, e.g. `/books/{id}/reviews`: normalised to single separators and only obtainable through
  * [[RoutePath.join]].
  */
opaque private[okapi] type RoutePath = String

private[okapi] object RoutePath {

  /** The template syntax. */
  enum Token(val text: String) {
    case Separator extends Token("/")
    case CaptureOpen extends Token("{")
    case CaptureClose extends Token("}")
  }

  /** A template segment, with its mark in [[specificityKey]]: fixed segments sort before captures. */
  enum Segment(val specificity: Char) {
    case Fixed(value: String) extends Segment('0')
    case Capture(name: String) extends Segment('1')
  }

  import Token.*

  /** `@Controller` base path + routing-annotation path. */
  def join(basePath: String, methodPath: String): RoutePath = {
    (basePath.stripSuffix(Separator.text) + Separator.text + methodPath.stripPrefix(Separator.text))
      .replaceAll(s"${Separator.text}+", Separator.text)
      .nn
  }

  extension (path: RoutePath) {
    def show: String = path

    def segments: List[Segment] = path.split(Separator.text).nn.toList.map(_.nn).filter(_.nonEmpty).map(segment)

    def captures: List[String] = segments.collect { case Segment.Capture(name) => name }

    /** Sort key putting more specific routes first: segment marks compared lexicographically, so `/books/stats` is
      * tried before `/books/{id}` and `/a/{x}` before `/{y}/b`.
      *
      * @param appendedCaptures
      *   captures added after the template (`@Path` parameters it does not mention).
      */
    def specificityKey(appendedCaptures: Int): String =
      (segments.map(_.specificity) ++ List.fill(appendedCaptures)(Segment.Capture("").specificity)).mkString
  }

  private def segment(text: String): Segment = {
    val isCapture = {
      text.length > CaptureOpen.text.length + CaptureClose.text.length &&
      text.startsWith(CaptureOpen.text) &&
      text.endsWith(CaptureClose.text)
    }
    if isCapture then
      Segment.Capture(text.substring(CaptureOpen.text.length, text.length - CaptureClose.text.length).nn)
    else Segment.Fixed(text)
  }
}
