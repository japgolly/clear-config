package japgolly.microlibs.stdlib_ext

import java.time.Duration
import java.time.temporal.ChronoUnit
import scala.util.matching.Regex

object ParseDouble {
  def unapply(s: String): Option[Double] =
    try {
      Some(s.toDouble)
    } catch {
      case _: java.lang.NumberFormatException => None
    }
}

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

object ParseChronoUnit {
  private def normalise(s: String): String =
    s.toLowerCase

  private val TextToChronoUnitMap: Map[String, ChronoUnit] = {
    var m = Map.empty[String, ChronoUnit]
    def add(u: ChronoUnit, others: String*): Unit =
      (others.toIterator ++ Iterator.single(u.toString))
        .map(normalise)
        .foreach(s => m = m.updated(s, u))
    ChronoUnit.values() foreach {
      case u@ ChronoUnit.NANOS     => add(u, "ns", "nano", "nanosecond", "nanoseconds")
      case u@ ChronoUnit.MICROS    => add(u, "μs", "micro", "microsecond", "microseconds")
      case u@ ChronoUnit.MILLIS    => add(u, "ms", "milli", "millisecond", "milliseconds")
      case u@ ChronoUnit.SECONDS   => add(u, "s", "sec", "second")
      case u@ ChronoUnit.MINUTES   => add(u, "min", "minute")
      case u@ ChronoUnit.HOURS     => add(u, "hr", "hour")
      case u@ ChronoUnit.HALF_DAYS => add(u, "halfday")
      case u@ ChronoUnit.DAYS      => add(u, "d", "day")
      case u@ ChronoUnit.WEEKS     => add(u, "w", "week")
      case u@ ChronoUnit.MONTHS    => add(u, "month")
      case u@ ChronoUnit.YEARS     => add(u, "y", "yr", "year")
      case u@ ChronoUnit.DECADES   => add(u, "decade")
      case u@ ChronoUnit.CENTURIES => add(u, "century")
      case u@ ChronoUnit.MILLENNIA => add(u, "millennium")
      case u@ ChronoUnit.ERAS      => add(u, "era")
      case u@ ChronoUnit.FOREVER   => add(u)
    }
    m
  }

  def unapply(s: String): Option[ChronoUnit] =
    TextToChronoUnitMap get normalise(s)
}

object ParseDuration {

  private val eachS = "(?:(-?\\d+)\\s*([a-zA-Z]+))"
  private val each = eachS.r
  private val all = s"$eachS(?:(?:\\s|,)*$eachS)*".r.pattern

  def unapply(s: String): Option[Duration] =
    if (all.matcher(s).matches)
      each.findAllMatchIn(s)
        .map(m => (m.group(1), m.group(2)) match {
          case (ParseLong(n), ParseChronoUnit(u)) => Some(u.getDuration multipliedBy n)
          // case (ParseDouble(n), ParseChronoUnit(u)) => Some(Duration ofNanos (u.getDuration.toNanos * n).toLong)
          case _ => None
        })
        .reduce[Option[Duration]] {
          case (Some(d1), Some(d2)) => Some(d1 plus d2)
          case _ => None
        }
    else
      None
}
