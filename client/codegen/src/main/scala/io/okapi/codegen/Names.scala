package io.okapi.codegen

/** A dot-separated Scala package name. */
opaque type PackageName = String

object PackageName {
  private val Valid = "[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*"

  def apply(name: String): Either[String, PackageName] =
    if (name.matches(Valid)) Right(name) else Left(s"'$name' is not a package name")

  extension (pkg: PackageName) {
    def value: String = pkg

    def sub(name: String): PackageName = s"$pkg.$name"

    /** The fully qualified name of `member` of this package. */
    def member(name: String): String = s"$pkg.$name"

    /** The path of the source file of `name` in this package, relative to the source root. */
    def file(name: String): String = pkg.replace('.', '/') + "/" + name + ".scala"
  }
}

/** A Scala identifier as it appears in source: backticked when it is a keyword. */
opaque type Identifier = String

object Identifier {
  private val Reserved = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
    "macro",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
  )

  /** `user-id` → `userId`. */
  def term(name: String): Identifier = escape(words(name) match {
    case Nil => "value"
    case first :: rest => leadingDigit(first.head.toLower.toString + first.tail + rest.map(_.capitalize).mkString)
  })

  /** `in-progress` → `InProgress`. */
  def tpe(name: String): Identifier = escape(words(name).map(_.capitalize).mkString match {
    case "" => "Value"
    case joined => leadingDigit(joined)
  })

  /** `name` capitalized when it is an identifier already, else [[tpe]]. */
  def tpeOrConverted(name: String): Identifier =
    if (name.matches("[A-Za-z_][A-Za-z0-9_]*")) escape(name.capitalize) else tpe(name)

  private def words(name: String): List[String] = name.split("[^A-Za-z0-9]+").toList.filter(_.nonEmpty)

  private def leadingDigit(name: String): String = if (name.head.isDigit) "_" + name else name

  private def escape(name: String): Identifier = if (Reserved.contains(name)) s"`$name`" else name

  /** The first of `name`, `name + suffix` (when `suffix` is not empty), `name2`, `name3`, … not in `taken`. */
  def unique(name: Identifier, taken: Set[Identifier], suffix: String = ""): Identifier = {
    val candidates = Iterator(name) ++ Iterator(name + suffix).filter(_ => suffix.nonEmpty) ++
      Iterator.from(2).map(i => s"$name$i")
    candidates.find(c => !taken.contains(c)).get
  }

  extension (id: Identifier) {
    def value: String = id

    /** The name without backticks. */
    def bare: String = id.stripPrefix("`").stripSuffix("`")

    def +(suffix: String): Identifier = escape(bare + suffix)
  }
}

/** A name as it is sent: a JSON field, a parameter, a header or a form field. */
opaque type WireName = String

object WireName {
  def apply(name: String): WireName = name

  extension (name: WireName) {
    def value: String = name

    /** The Scala string literal of the name. */
    def literal: String = Literal(name)
  }
}

/** The Scala string literal of `text`. */
object Literal {
  def apply(text: String): String = {
    val escaped = text.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    }
    "\"" + escaped + "\""
  }
}
