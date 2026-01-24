package japgolly.clearconfig.internals

import cats.syntax.all._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

trait LogbackFunctions {

  /** Scans logback xml files for environment variables so that they can appear in a config report.
    *
    * Support for nested substitutions is limited.
    * If a nested substitution cannot be parsed, a warning will be emitted to stderr, in which case
    * you are advised to write your own ConfigDef composition rather than relying on this method.
    */
  def logbackXmlOnClasspath: ConfigDef[Unit] =
    logbackXmlOnClasspath("logback-test.xml", "logback.xml")

  /** Scans logback xml files for environment variables so that they can appear in a config report.
    *
    * Support for nested substitutions is limited.
    * If a nested substitution cannot be parsed, a warning will be emitted to stderr, in which case
    * you are advised to write your own ConfigDef composition rather than relying on this method.
    */
  def logbackXmlOnClasspath(filename: String, otherFilenames: String*): ConfigDef[Unit] = {
    @tailrec
    def go(filenames: List[String]): ConfigDef[Unit] =
      filenames match {
        case Nil => ConfigDef.unit
        case filename :: rest =>
          readResource(filename.replaceFirst("^/*", "/")) match {
            case Some(xml) => logbackXmlContent(xml)
            case None      => go(rest)
          }
      }

    go(filename :: otherFilenames.toList)
  }

  private def readResource(filename: String): Option[String] =
    Option(getClass.getResourceAsStream(filename)).map { is =>
      scala.io.Source.fromInputStream(is, "UTF-8").mkString
    }

  /** Scans logback xml files for environment variables so that they can appear in a config report.
    *
    * Support for nested substitutions is limited.
    * If a nested substitution cannot be parsed, a warning will be emitted to stderr, in which case
    * you are advised to write your own ConfigDef composition rather than relying on this method.
    */
  def logbackXmlContent(content: String): ConfigDef[Unit] = {
    var result = ConfigDef.unit
    val GetOrElse = "(.*?):-(.*)".r

    def parseExpr(expr: String): Option[String] = {
      val body = expr.drop(2).dropRight(1) // Remove "${" and "}"

      body match {
        case GetOrElse(lhs, rhs) =>
          if (Logback.extractSubstitutions(lhs).nonEmpty) {
            Logback.warn("Ignoring unsupported expression: " + lhs)
            None
          } else {
            val default = parseDefault(rhs)
            default match {
              case Some(d) => result = ConfigDef.getOrUse(lhs, d) *> result
              case None    => result = ConfigDef.get[String](lhs) *> result
            }
            default
          }

        case _ =>
          if (Logback.extractSubstitutions(body).nonEmpty)
            Logback.warn("Ignoring unsupported expression: " + body)
          else
            result = ConfigDef.need[String](body) *> result
          None
      }
    }

    def parseDefault(expr: String): Option[String] =
      if (Logback.extractSubstitutions(expr).isEmpty)
        Some(expr)
      else
        parseExpr(expr)

    for (expr <- Logback.extractSubstitutions(content))
      parseExpr(expr)

    result
  }
}

object Logback {

  def extractSubstitutions(input: String): List[String] = {
    val results = ListBuffer.empty[String]
    var depth = 0
    var startIndex = -1
    var i = 0

    while (i < input.length) {
      val char = input(i)

      if (depth == 0) {
        // We are searching for a new start "${"
        if (char == '$' && i + 1 < input.length && input(i + 1) == '{') {
          startIndex = i
          depth = 1
          i += 1 // Skip the '{' to avoid counting it as a nested open brace immediately
        }
      } else {
        // We are currently inside a variable declaration
        if (char == '{') {
          depth += 1
        } else if (char == '}') {
          depth -= 1
          // If we are back to 0, we found the closing brace for the top-level var
          if (depth == 0) {
            results += input.substring(startIndex, i + 1)
          }
        }
      }
      i += 1
    }

    results.toList
  }

  def warn(msg: String): Unit =
    System.err.println("[clear-config warning] " + msg)

}
