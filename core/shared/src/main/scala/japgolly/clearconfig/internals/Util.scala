package japgolly.clearconfig.internals

private[internals] object Util {

  def fmtList(one: String, other: String, items: Vector[String]): String = {
    val n = items.length
    val a = if (n == 1) one else other
    s"$n $a:${items.iterator.map("\n  - " + _).mkString}"
  }

  def fmtSourceNameList(sourcesHighToLowPri: Vector[SourceName]): String =
    fmtList("source", "sources (highest to lowest priority)", sourcesHighToLowPri.map(_.value))

}
