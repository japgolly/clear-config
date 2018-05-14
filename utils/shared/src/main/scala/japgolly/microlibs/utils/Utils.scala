package japgolly.microlibs.utils

import japgolly.univeq.UnivEq
import japgolly.microlibs.stdlib_ext.StdlibExt._

object Utils {

  def dups[A: UnivEq](as: TraversableOnce[A]): Iterator[A] = {
    val seen = collection.mutable.HashSet.empty[A]
    as.toIterator.map { a =>
      if (seen contains a)
        Some(a)
      else {
        seen += a
        None
      }
    }.filterDefined
  }

  def quickStringExists(strings: Set[String]): String => Boolean = {
    val maxLen = strings.foldLeft(0)(_ max _.length)
    val byLen = Array.fill(maxLen + 1)(Set.empty[String])
    strings.foreach(s => byLen(s.length) = byLen(s.length) + s)
    s => (s.length <= maxLen) && byLen(s.length).contains(s)
  }

  // Improved a larger benchmark by 8.6%
  def quickStringLookup[A](map: Map[String, A]): String => Option[A] = {
    val strings = map.keySet
    val maxLen = strings.foldLeft(0)(_ max _.length)
    val byLen = Array.fill(maxLen + 1)(Map.empty[String, A])
    strings.foreach(s => byLen(s.length) = byLen(s.length).updated(s, map(s)))
    s => if (s.length <= maxLen) byLen(s.length).get(s) else None
  }

  /**
    * Space = Θ(mn)
    * Time  = Θ(nᵐ)
    */
  def levenshtein(str1: String, str2: String): Int = {
    val m = str1.length
    val n = str2.length

    val d: Array[Array[Int]] = Array.ofDim(m + 1, n + 1)
    for (i <- 0 to m) d(i)(0) = i
    for (j <- 0 to n) d(0)(j) = j

    for (i <- 1 to m; j <- 1 to n) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1
      val a = d(i-1)(j  ) + 1     // deletion
      val b = d(i  )(j-1) + 1     // insertion
      val c = d(i-1)(j-1) + cost  // substitution
      d(i)(j) = a min b min c
    }

    d(m)(n)
  }

  /**
   * Pattern.quote doesn't work in Scala.JS.
   *
   * http://stackoverflow.com/questions/2593637/how-to-escape-regular-expression-in-javascript
   */
  def regexEscape(s: String): String = {
    var r = s
    r = regexEscape1.replaceAllIn(r, """\\$1""")
    r = regexEscape2.replaceAllIn(r, """\\x08""")
    r
  }

  private[this] val regexEscape1 = """([-()\[\]{}+?*.$\^|,:#<!\\])""".r
  private[this] val regexEscape2 = """\x08""".r

  def regexEscapeAndWrap(s: String): String =
    s"(?:${regexEscape(s)})"
}
