package japgolly.clearconfig.internals

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.AsciiTable
import java.util.regex.Pattern
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

  final case class Colours(secret: String, error: String, reset: String)

  object Colours {
    def off = apply("", "", "")

    def on = {
      import scala.Console._
      apply(YELLOW, RED, RESET)
    }
  }

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

  final case class ValueDisplay(fmt: (Colours, SourceName, Key, String) => String) {
    def +(f: ValueDisplay): ValueDisplay =
      ValueDisplay((c, s, k, v) => f.fmt(c, s, k, fmt(c, s, k, v)))

    def map(f: String => String): ValueDisplay =
      ValueDisplay((c, s, k, v) => f(fmt(c, s, k, v)))

    def when(cond: (SourceName, Key, String) => Boolean): ValueDisplay =
      ValueDisplay((c, s, k, v) => if (cond(s, k, v)) fmt(c, s, k, v) else v)

    def unless(cond: (SourceName, Key, String) => Boolean): ValueDisplay =
      when(!cond)
  }

  object ValueDisplay {
    def identity: ValueDisplay =
      ValueDisplay((_, _, _, v) => v)

    def escapeCtrlChars: ValueDisplay =
      ValueDisplay((_, _, _, v) => v.toIterator.flatMap {
        case '\b' => "\\b"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case '\f' => "\\f"
        case c => c.toString
      }.mkString)

    def limitWidth(maxLen: Int): ValueDisplay =
      ValueDisplay((_, _, _, v) => if (v.length <= maxLen) v else v.take(maxLen - 1) + "…")

    def obfuscate: ValueDisplay =
      ValueDisplay((c, _, _, v) => "%s<# %08X #>%s".format(c.secret, MurmurHash3 stringHash v, c.reset))

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

    def report(sourcesHighToLowPri: Vector[SourceName], c: Colours, vd: ValueDisplay): String =
      if (isEmpty)
        "No data to report."
      else {
        def fmtError(e: String) = s"${c.error}$e${c.reset}"

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
                case Lookup.Found(_, v)         => vd.fmt(c, s, k, v)
                case Lookup.NotFound            => ""
                case Lookup.Error(err, None)    => fmtError(err)
                case Lookup.Error(err, Some(v)) => s"${vd.fmt(c, s, k, v)} ${fmtError(err)}"
              })
            }
        AsciiTable(header :: valueRows)
      }
  }

  // ===================================================================================================================

  final case class Settings(prepareUsed   : SubReport => SubReport,
                            prepareUnused : SubReport => SubReport,
                            colours       : Colours,
                            valueDisplay0 : ValueDisplay,
                            maxValueLen   : Option[Int],
                            showSourceList: Boolean,
                            usedHeader    : String,
                            unusedHeader  : String)
  object Settings {
    def default: Settings =
      apply(
        _.withoutEmptySourceCols,
        _.withoutKeys("PROMPT", "PS1").filterKeysNot(_ contains "TERMCAP"),
        Colours.off,
        ValueDisplay.default,
        Some(64),
        true,
        "Used keys",
        "Unused keys")
  }

  // ===================================================================================================================

  def apply(sourcesHighToLowPri: Vector[SourceName],
            used               : Map[Key, Map[SourceName, Lookup]],
            unused             : Map[Key, Map[SourceName, Lookup]],
            settings           : Settings): Report =
    Report(
      sourcesHighToLowPri,
      settings.prepareUsed(SubReport(Table(used))),
      settings.prepareUnused(SubReport(Table(unused))),
      settings.colours,
      settings.valueDisplay0,
      settings.maxValueLen,
      settings.showSourceList,
      settings.usedHeader,
      settings.unusedHeader)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

import Report._

final case class Report(sourcesHighToLowPri: Vector[SourceName],
                        usedReport         : SubReport,
                        unusedReport       : SubReport,
                        colours            : Colours,
                        valueDisplay0      : ValueDisplay,
                        maxValueLen        : Option[Int],
                        showSourceList     : Boolean,
                        usedHeader         : String,
                        unusedHeader       : String) extends HasTable[Report] {

  override def toString = "ConfigReport"

  def valueDisplay: ValueDisplay =
    maxValueLen.fold(valueDisplay0)(ValueDisplay.limitWidth(_) + valueDisplay0)

  override def map(f: Table => Table) =
    copy(usedReport = usedReport.map(f), unusedReport = unusedReport.map(f))

  private def subReport(showHeader: Boolean, header: String, sr: SubReport): String = {
    val r = sr.report(sourcesHighToLowPri, colours, valueDisplay)
    if (showHeader) s"$header (${sr.size}):\n$r" else r
  }

  def sources: String = Util.fmtSourceNameList(sourcesHighToLowPri)

  def used: String = used(true)
  def unused: String = unused(true)

  def used(withHeader: Boolean): String = subReport(withHeader, usedHeader, usedReport)
  def unused(withHeader: Boolean): String = subReport(withHeader, unusedHeader, unusedReport)

  def full: String =
    s"""
       !${if (showSourceList) sources else ""}
       !
       !${used(true)}
       !
       !${unused(true)}
     """.stripMargin('!').trim

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Customisation

  def mapUsed(f: SubReport => SubReport): Report =
    copy(usedReport = f(usedReport))

  def mapUnused(f: SubReport => SubReport): Report =
    copy(unusedReport = f(unusedReport))

  def withValueDisplay(f: ValueDisplay => ValueDisplay): Report =
    copy(valueDisplay0 = f(valueDisplay0))

  def withMaxValueLength(i: Int): Report =
    copy(maxValueLen = Some(i))

  /** Convenient shortcut because this is such a common case. */
  def obfuscateKeys(f: Key => Boolean): Report =
    withValueDisplay(_ + ValueDisplay.obfuscateKeys(f))

  def withColour(enable: Boolean): Report = {
    val c = enable match {
      case true if colours == Colours.off => Colours.on
      case true                           => colours
      case false                          => Colours.off
    }
    copy(colours = c)
  }

  def withColour: Report =
    withColour(true)

  def withoutColour: Report =
    withColour(false)

}
