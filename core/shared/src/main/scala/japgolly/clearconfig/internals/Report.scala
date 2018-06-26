package japgolly.clearconfig.internals

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.AsciiTable
import java.util.regex.Pattern
import scala.Console._
import scala.util.hashing.MurmurHash3

object Report {

  private def isBlank(m: Map[_, Lookup]): Boolean =
    m.forall(_._2 == Lookup.NotFound)

  private def swap[A, B, V](m: Map[A, Map[B, V]]): Map[B, Map[A, V]] =
      m.iterator
        .flatMap { case (a, bv) => bv.iterator.map { case (s, v) => (s, (a, v)) } }
        .toList
        .groupBy(_._1)
        .mapValuesNow(_.iterator.map(_._2).toMap)

  final case class Table(byKey: Map[Key, Map[SourceName, Lookup]]) {

    val bySource: Map[SourceName, Map[Key, Lookup]] =
      swap(byKey)

    def isEmpty = byKey.isEmpty
    def nonEmpty = !isEmpty

    def modByKey(f: Map[Key, Map[SourceName, Lookup]] => Map[Key, Map[SourceName, Lookup]]): Table =
      Table(f(byKey))

    def modBySource(f: Map[SourceName, Map[Key, Lookup]] => Map[SourceName, Map[Key, Lookup]]): Table =
      Table(swap(f(bySource)))
  }

  trait HasTable[A <: HasTable[A]] {
    def map(f: Table => Table): A

    final def filterKeys(f: String => Boolean): A =
      map(_.modByKey(_.filterKeys(k => f(k.value))))

    final def filterKeysNot(f: String => Boolean): A =
      filterKeys(!f(_))

    final def withoutKeys(keys: String*): A = {
      val keySet = keys.toSet
      filterKeys(!keySet.contains(_))
    }

    final def filterSources(f: SourceName => Boolean): A =
      map(_.modBySource(_.filterKeys(f)))

    final def filterSourcesNot(f: SourceName => Boolean): A =
      filterSources(!f(_))

    final def withoutSources(sources: SourceName*): A = {
      val excludeSet = sources.toSet
      filterSources(!excludeSet.contains(_))
    }

    final def withoutEmptySourceCols: A =
      map(_.modBySource(_.filter(x => !isBlank(x._2))))

    final def withoutEmptyKeyRows: A =
      map(_.modByKey(_.filter(x => !isBlank(x._2))))
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
      Pattern.compile(".*(?:credential|password|secret).*", Pattern.CASE_INSENSITIVE)
  }

  // ===================================================================================================================

  final case class SubReport(table: Table) extends HasTable[SubReport] {

    def isEmpty = table.isEmpty
    def nonEmpty = !isEmpty
    def size = table.byKey.size

    override def map(f: Table => Table) =
      SubReport(f(table))

    def report(sourcesHighToLowPri: Vector[SourceName], valueDisplay: ValueDisplay): String =
      if (isEmpty)
        "No data to report."
      else {
        def fmtError(e: String) = s"$RED$e$RESET"

        val sources: Vector[SourceName] =
          sourcesHighToLowPri.filter(table.bySource.contains)

        val header: Vector[String] =
          "Key" +: sources.map(_.value)

        val valueRows: List[Vector[String]] =
          table.byKey.iterator
            .toList
            .sortBy(_._1.value)
            .map { case (k, vs) =>
              k.value +: sources.map(s => vs.getOrElse(s, Lookup.NotFound) match {
                case Lookup.Found(_, v)         => valueDisplay.fmt(s, k, v)
                case Lookup.NotFound            => ""
                case Lookup.Error(err, None)    => fmtError(err)
                case Lookup.Error(err, Some(v)) => s"${valueDisplay.fmt(s, k, v)} ${fmtError(err)}"
              })
            }
        AsciiTable(header :: valueRows)
      }
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
                   used               : Map[Key, Map[SourceName, Lookup]],
                   unused             : Map[Key, Map[SourceName, Lookup]]): Report =
    Report(
      sourcesHighToLowPri,
      SubReport(Table(used)).withoutEmptySourceCols,
      SubReport(Table(unused)).withoutKeys("PROMPT", "PS1").filterKeysNot(_ contains "TERMCAP"),
      Settings.default)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

import Report._

final case class Report(sourcesHighToLowPri: Vector[SourceName],
                        used               : SubReport,
                        unused             : SubReport,
                        settings           : Settings) extends HasTable[Report] {
  override def toString = "Report"

  override def map(f: Table => Table) =
    copy(used = used.map(f), unused = unused.map(f))

  def reportUsed: String =
    used.report(sourcesHighToLowPri, settings.display)

  def reportUnused: String =
    unused.report(sourcesHighToLowPri, settings.display)

  def report: String =
    s"""
       !${if (settings.showSourceList) Util.fmtSourceNameList(sourcesHighToLowPri) else ""}
       !
       !Used keys (${used.size}):
       !$reportUsed
       !
       !Unused keys (${unused.size}):
       !$reportUnused
     """.stripMargin('!').trim

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Customisation

  def mapUsed(f: SubReport => SubReport): Report =
    copy(used = f(used))

  def mapUnused(f: SubReport => SubReport): Report =
    copy(unused = f(unused))

  def withSettings(f: Settings => Settings): Report =
    copy(settings = f(settings))

  def withValueDisplay(f: ValueDisplay => ValueDisplay): Report =
    copy(settings = settings.copy(display0 = f(settings.display0)))

  def withMaxValueLength(i: Int): Report =
    withSettings(_.copy(maxValueLen = Some(i)))

  /** Convenient shortcut because this is such a common case. */
  def obfuscateKeys(f: Key => Boolean): Report =
    withValueDisplay(_ + ValueDisplay.obfuscateKeys(f))
}
