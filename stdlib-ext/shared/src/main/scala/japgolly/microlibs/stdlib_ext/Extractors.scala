package japgolly.microlibs.stdlib_ext

object ParseLong {
  def unapply(s: String): Option[Long] =
    try {
      Some(s.toLong)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
}

object ParseInt {
  def unapply(s: String): Option[Int] =
    try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
}