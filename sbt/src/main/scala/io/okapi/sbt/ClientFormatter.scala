package io.okapi.sbt

import java.io.{ File, OutputStreamWriter, PrintWriter }
import java.nio.file.Path

import org.scalafmt.interfaces.{ Scalafmt, ScalafmtReporter }
import sbt.io.IO

/** Formats the generated client with scalafmt, in the version the configuration names (downloaded on first use). A
  * source scalafmt cannot parse fails the task with scalafmt's error.
  */
private[sbt] object ClientFormatter {

  /** The configuration used when there is no `.scalafmt.conf`. */
  val DefaultConfig: String = {
    s"""version = "${OkapiVersions.scalafmt}"
       |runner.dialect = scala3
       |style = defaultWithAlign
       |align { stripMargin = true, preset = none }
       |assumeStandardLibraryStripMargin = false
       |binPack.literalArgumentLists = false
       |continuationIndent { defnSite = 2, ctorSite = 2, extendSite = 2, withSiteRelativeToExtends = 2 }
       |includeNoParensInSelectChains = false
       |optIn.breakChainOnFirstMethodDot = false
       |indent.caseSite = 5
       |indentOperator.topLevelOnly = false
       |maxColumn = 120
       |newlines { alwaysBeforeElseAfterCurlyIf = true, avoidInResultType = true, beforeCurlyLambdaParams = multilineWithCaseOnly }
       |rewrite {
       |  rules = [PreferCurlyFors, RedundantParens, SortModifiers, Imports]
       |  imports { sort = scalastyle, contiguousGroups = only, groups = [["zio.*"], ["api.*", "domain.*", "implementation.*"], ["java.*"]] }
       |  sortModifiers.order = ["override", "private", "protected", "implicit", "final", "sealed", "abstract", "lazy"]
       |}
       |spaces.inImportCurlyBraces = true
       |trailingCommas = multiple
       |danglingParentheses.exclude = []
       |verticalMultiline { arityThreshold = 7, atDefnSite = true, newlineAfterOpenParen = true }
       |""".stripMargin
  }

  /** @param fallback
    *   where [[DefaultConfig]] is written when `config` is `None`.
    */
  def format(files: Seq[File], config: Option[File], fallback: File): Unit = {
    val configPath = config.getOrElse { IO.write(fallback, DefaultConfig); fallback }.toPath
    val thread = Thread.currentThread
    val previous = thread.getContextClassLoader
    // scalafmt's downloader finds its implementation through the context class loader, which sbt sets to its own
    thread.setContextClassLoader(getClass.getClassLoader)
    try {
      val scalafmt = Scalafmt.create(getClass.getClassLoader).withReporter(Reporter)
      files.foreach(file => IO.write(file, scalafmt.format(configPath, file.toPath, IO.read(file))))
    }
    finally thread.setContextClassLoader(previous)
  }

  private object Reporter extends ScalafmtReporter {
    def error(file: Path, message: String): Unit = fail(file, message)

    def error(file: Path, e: Throwable): Unit = {
      val causes =
        Iterator.iterate(e)(_.getCause).takeWhile(_ != null).map(c => s"${c.getClass.getName}: ${c.getMessage}")
      fail(file, causes.mkString("; caused by "))
    }

    private def fail(file: Path, cause: String): Nothing =
      sys.error(
        s"scalafmt failed on $file ($cause); the client is generated, but this file and the ones after it are not " +
          "formatted. Check okapiClientScalafmtConfig, or report the generated source if it does not parse."
      )

    def excluded(file: Path): Unit = ()

    def parsedConfig(config: Path, scalafmtVersion: String): Unit = ()

    def downloadWriter(): PrintWriter = new PrintWriter(System.err)

    def downloadOutputStreamWriter(): OutputStreamWriter = new OutputStreamWriter(System.err)
  }
}
