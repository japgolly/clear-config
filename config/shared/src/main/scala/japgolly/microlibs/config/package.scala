package japgolly.microlibs

package object config {

  private[config] def keyModTS(f: Key => Key): String => String = s => f(Key(s)).value
  private[config] def keyModFS(f: String => String): Key => Key = k => Key(f(k.value))

  private[config] def fmtList(one: String, other: String, items: Vector[String]): String = {
    val n = items.length
    val a = if (n == 1) one else other
    s"$n $a:${items.iterator.map("\n  - " + _).mkString}"
  }

  private[config] def fmtSourceNameList(sourcesHighToLowPri: Vector[SourceName]): String =
    fmtList("source", "sources (highest to lowest priority)", sourcesHighToLowPri.map(_.value))
}
