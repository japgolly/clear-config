package japgolly.microlibs.utils

/**
 * Format a set of ints into a concise textual description.
 *
 * Example:
 *    "2, 5, 7-14, 20"
 */
object ConciseIntSetFormat {
  private sealed trait Tmp
  private final case class N(n: Int) extends Tmp
  private final case class R(from: Int, to: Int) extends Tmp

  def apply(ints: Set[Int], sep: String = ",", rangeSep: String = "-"): String =
    if (ints.isEmpty)
      ""
    else {

      val comps = ints.toArray.sorted.foldRight(List.empty[Tmp])((i, cs) =>
        cs match {
          case N(a) :: N(b) :: t if i == a - 1 && a == b - 1 =>
            R(i, b) :: t
          case R(a, b)     :: t if i == a - 1 =>
            R(i, b) :: t
          case _ =>
            N(i) :: cs
        })

      val sb = new StringBuilder
      for (c <- comps) {
        if (sb.nonEmpty) sb.append(sep)
        c match {
          case n: N => sb append n.n
          case r: R => sb append r.from; sb append rangeSep; sb append r.to
        }
      }
      sb.toString
    }
}
