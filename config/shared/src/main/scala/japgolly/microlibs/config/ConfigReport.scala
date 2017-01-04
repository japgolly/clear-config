package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext.AsciiTable
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.Console._
import ConfigReport._

object ConfigReport {

  final case class RowFilter(allow: (Key, Map[SourceName, ConfigValue]) => Boolean) extends AnyVal {
    def unary_! : RowFilter = RowFilter(!allow)
    def &&(f: RowFilter): RowFilter = RowFilter(allow && f.allow)
    def ||(f: RowFilter): RowFilter = RowFilter(allow || f.allow)
    def addExclusion(f: RowFilter): RowFilter = RowFilter(allow && !f.allow)
  }

  object RowFilter {
    def allowAll: RowFilter =
      RowFilter((_, _) => true)

    def exclude(f: (Key, Map[SourceName, ConfigValue]) => Boolean): RowFilter =
      RowFilter(!f)

    /** Exclude rows where a key is only provided by a specified single source. */
    def excludeWhereSingleSource(s: SourceName): RowFilter =
      excludeWhereSingleSource(_ == s)

    /** Exclude rows where a key is only provided by a single source, and that source matches given criteria. */
    def excludeWhereSingleSource(f: SourceName => Boolean): RowFilter =
      exclude((_, vs) => vs.size == 1 && f(vs.keysIterator.next()))

    def excludeKeys(keys: String*): RowFilter =
      excludeByKey(keys.toIterator.map(Key).toSet.contains)

    def excludeByKey(f: Key => Boolean): RowFilter =
      exclude((k, _) => f(k))

    def defaultForUsedReport: RowFilter =
      allowAll

    def defaultForUnusedReport: RowFilter =
      excludeByKey(_.value contains "TERMCAP") &&
      excludeKeys("PROMPT", "PS1")
  }

  // ===================================================================================================================

  final case class ValueDisplay(fmt: (Key, String) => String) extends AnyVal {
    def +(f: ValueDisplay): ValueDisplay =
      ValueDisplay((k, s) => f.fmt(k, fmt(k, s)))

    def contramapKeys(f: String => String): ValueDisplay = {
      val g = keyModFS(f)
      ValueDisplay((k, s) => fmt(g(k), s))
    }
  }

  object ValueDisplay {
    def identity: ValueDisplay =
      ValueDisplay((_, s) => s)

    def escapeCtrlChars: ValueDisplay =
      ValueDisplay((_, s) => s.toIterator.flatMap {
        case '\b' => "\\b"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case '\f' => "\\f"
        case c => c.toString
      }.mkString)

    def limitWidth(maxLen: Int): ValueDisplay =
      ValueDisplay((_, s) => if (s.length <= maxLen) s else s.take(maxLen - 1) + "…")

    def obfuscateKey(f: String => Boolean): ValueDisplay =
      ValueDisplay((k, s) => if (f(k.value)) s"$YELLOW<# %08X #>$RESET".format(s.##) else s)

    def default: ValueDisplay =
      escapeCtrlChars +
      (obfuscateKey(_ contains "password") + obfuscateKey(_ contains "secret")).contramapKeys(_.toLowerCase)
  }

  // ===================================================================================================================

  final case class SubReport(data: Map[Key, Map[SourceName, ConfigValue]], rowFilter: RowFilter) {

    def report(sourcesHighToLowPri: Vector[SourceName], valueDisplay: ValueDisplay): String = {
      val header: Vector[String] =
        "Key" +: sourcesHighToLowPri.map(_.value)

      def fmtError(e: String) = s"$RED$e$RESET"

      val valueRows: List[Vector[String]] =
        data.iterator
          .filter(rowFilter.allow.tupled)
          .toList
          .sortBy(_._1.value)
          .map { case (k, vs) =>
            def fmtValue(v: String) = valueDisplay.fmt(k, v)
            k.value +: sourcesHighToLowPri.map(vs.getOrElse(_, ConfigValue.NotFound)).map {
              case ConfigValue.Found(v)            => fmtValue(v)
              case ConfigValue.NotFound            => ""
              case ConfigValue.Error(err, None)    => fmtError(err)
              case ConfigValue.Error(err, Some(v)) => s"${fmtValue(v)} ${fmtError(err)}"
            }
          }
      AsciiTable(header :: valueRows)
    }

    def addRowFilter(f: RowFilter): SubReport =
      copy(rowFilter = rowFilter && f)
  }

  // ===================================================================================================================

  final case class Settings(display0      : ValueDisplay,
                            maxValueLen   : Option[Int],
                            showSourceList: Boolean) {
    def display: ValueDisplay =
      maxValueLen.fold(display0)(ValueDisplay.limitWidth(_) + display0)
  }

  object Settings {
    def default: Settings =
      Settings(ValueDisplay.default, Some(64), true)
  }

  // ===================================================================================================================

  def withDefaults(sourcesHighToLowPri: Vector[SourceName],
                   used               : Map[Key, Map[SourceName, ConfigValue]],
                   unused             : Map[Key, Map[SourceName, ConfigValue]]): ConfigReport =
    ConfigReport(
      sourcesHighToLowPri,
      SubReport(used, RowFilter.defaultForUsedReport),
      SubReport(unused, RowFilter.defaultForUnusedReport),
      Settings.default)
}

final case class ConfigReport(sourcesHighToLowPri: Vector[SourceName],
                              used               : SubReport,
                              unused             : SubReport,
                              settings           : Settings) {
  override def toString = "ConfigReport"

  def reportUsed: String =
    used.report(sourcesHighToLowPri, settings.display)

  def reportUnused: String =
    unused.report(sourcesHighToLowPri, settings.display)

  def report: String =
    s"""
       !${if (settings.showSourceList) fmtSourceNameList(sourcesHighToLowPri) else ""}
       !
       !Used keys (${used.data.size}):
       !$reportUsed
       !
       !Unused keys (${unused.data.size}):
       !$reportUnused
     """.stripMargin('!').trim

  def withUsedSettings(f: SubReport => SubReport): ConfigReport =
    copy(used = f(used))

  def withUnusedSettings(f: SubReport => SubReport): ConfigReport =
    copy(unused = f(unused))

  def withSettings(f: Settings => Settings): ConfigReport =
    copy(settings = f(settings))

  def withMaxValueLength(i: Int): ConfigReport =
    withSettings(_.copy(maxValueLen = Some(i)))
}
