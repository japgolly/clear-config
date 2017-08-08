package japgolly.microlibs.config

import japgolly.microlibs.config.ConfigReport._
import japgolly.microlibs.stdlib_ext.AsciiTable
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.util.regex.Pattern
import scala.Console._
import scala.util.hashing.MurmurHash3

object ConfigReport {

  private def isValueMapEmpty(m: Map[_, ConfigValue]): Boolean =
    !m.exists(_._2 != ConfigValue.NotFound)

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

    def excludeEmpty: RowFilter =
      exclude((_, vs) => isValueMapEmpty(vs))

    def excludeKeys(keys: String*): RowFilter =
      excludeByKey(keys.toIterator.map(Key).toSet.contains)

    def excludeByKey(f: Key => Boolean): RowFilter =
      exclude((k, _) => f(k))

    /** Exclude rows where a key is only provided by a specified single source. */
    def excludeWhereSingleSource(s: SourceName): RowFilter =
      excludeWhereSingleSource(_ == s)

    /** Exclude rows where a key is only provided by a single source, and that source matches given criteria. */
    def excludeWhereSingleSource(f: SourceName => Boolean): RowFilter =
      exclude((_, vs) => vs.size == 1 && f(vs.keysIterator.next()))

    def defaultForUsedReport: RowFilter =
      allowAll

    def defaultForUnusedReport: RowFilter =
      excludeByKey(_.value contains "TERMCAP") &&
      excludeKeys("PROMPT", "PS1")
  }

  // ===================================================================================================================

  final case class ColFilter(allow: (SourceName, Map[Key, ConfigValue]) => Boolean) extends AnyVal {
    def unary_! : ColFilter = ColFilter(!allow)
    def &&(f: ColFilter): ColFilter = ColFilter(allow && f.allow)
    def ||(f: ColFilter): ColFilter = ColFilter(allow || f.allow)
    def addExclusion(f: ColFilter): ColFilter = ColFilter(allow && !f.allow)
  }

  object ColFilter {
    def allowAll: ColFilter =
      ColFilter((_, _) => true)

    def exclude(f: (SourceName, Map[Key, ConfigValue]) => Boolean): ColFilter =
      ColFilter(!f)

    def excludeEmpty: ColFilter =
      exclude((_, vs) => isValueMapEmpty(vs))

    def excludeSources(sources: SourceName*): ColFilter =
      excludeBySource(sources.toSet.contains)

    def excludeBySource(f: SourceName => Boolean): ColFilter =
      exclude((s, _) => f(s))

    def defaultForUsedReport: ColFilter =
      excludeEmpty

    def defaultForUnusedReport: ColFilter =
      excludeEmpty
  }

  // ===================================================================================================================

  final case class ValueDisplay(fmt: (SourceName, Key, String) => String) extends AnyVal {
    def +(f: ValueDisplay): ValueDisplay =
      ValueDisplay((s, k, v) => f.fmt(s, k, fmt(s, k, v)))

    def map(f: String => String): ValueDisplay =
      ValueDisplay((s, k, v) => f(fmt(s, k, v)))

    def when(cond: (SourceName, Key, String) => Boolean): ValueDisplay =
      ValueDisplay((s, k, v) => if (cond(s, k, v)) fmt(s, k, v) else v)

    def unless(cond: (SourceName, Key, String) => Boolean): ValueDisplay =
      when(!cond)
  }

  object ValueDisplay {
    def identity: ValueDisplay =
      ValueDisplay((_, _, v) => v)

    def escapeCtrlChars: ValueDisplay =
      ValueDisplay((_, _, v) => v.toIterator.flatMap {
        case '\b' => "\\b"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case '\f' => "\\f"
        case c => c.toString
      }.mkString)

    def limitWidth(maxLen: Int): ValueDisplay =
      ValueDisplay((_, _, v) => if (v.length <= maxLen) v else v.take(maxLen - 1) + "…")

    def obfuscate: ValueDisplay =
      ValueDisplay((_, _, v) => s"$YELLOW<# %08X #>$RESET".format(MurmurHash3 stringHash v))

    def obfuscateSources(f: SourceName => Boolean): ValueDisplay =
      obfuscate.when((s, _, _) => f(s))

    def obfuscateKeys(f: Key => Boolean): ValueDisplay =
      obfuscate.when((_, k, _) => f(k))

    def obfuscateSourcesAndKeys(f: String => Boolean): ValueDisplay =
      obfuscate.when((s, k, _) => f(s.value) || f(k.value))

    def default: ValueDisplay =
      escapeCtrlChars +
      obfuscateSourcesAndKeys(seemsSecret.matcher(_).matches)

    val seemsSecret: Pattern =
      Pattern.compile(".*(?:password|secret).*", Pattern.CASE_INSENSITIVE)
  }

  // ===================================================================================================================

  final case class SubReport(data     : Map[Key, Map[SourceName, ConfigValue]],
                             rowFilter: RowFilter,
                             colFilter: ColFilter) {

    def report(sourcesHighToLowPri: Vector[SourceName], valueDisplay: ValueDisplay): String =
      if (data.isEmpty)
        "No data to report."
      else {
        def fmtError(e: String) = s"$RED$e$RESET"

        val dataBySource: Map[SourceName, Map[Key, ConfigValue]] =
          data.iterator
            .flatMap { case (k, sv) => sv.iterator.map { case (s, v) => (s, (k, v)) } }
            .toList
            .groupBy(_._1)
            .mapValuesNow(_.iterator.map(_._2).toMap)

        val sources: Vector[SourceName] =
          sourcesHighToLowPri.filter(s => colFilter.allow(s, dataBySource.getOrElse(s, Map.empty)))

        val header: Vector[String] =
          "Key" +: sources.map(_.value)

        val valueRows: List[Vector[String]] =
          data.iterator
            .filter(rowFilter.allow.tupled)
            .toList
            .sortBy(_._1.value)
            .map { case (k, vs) =>
              k.value +: sources.map(s => vs.getOrElse(s, ConfigValue.NotFound) match {
                case ConfigValue.Found(v)            => valueDisplay.fmt(s, k, v)
                case ConfigValue.NotFound            => ""
                case ConfigValue.Error(err, None)    => fmtError(err)
                case ConfigValue.Error(err, Some(v)) => s"${valueDisplay.fmt(s, k, v)} ${fmtError(err)}"
              })
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
      SubReport(used, RowFilter.defaultForUsedReport, ColFilter.defaultForUsedReport),
      SubReport(unused, RowFilter.defaultForUnusedReport, ColFilter.defaultForUnusedReport),
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
