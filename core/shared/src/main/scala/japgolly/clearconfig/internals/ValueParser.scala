package japgolly.clearconfig.internals

import japgolly.microlibs.stdlib_ext._
import java.time._
import java.time.temporal.ChronoUnit
import java.util.regex.Pattern
import scala.concurrent.duration.FiniteDuration
import scalaz.{-\/, Alternative, Monad, \/, \/-}

final class ValueParser[A](val parse: Lookup.Found => String \/ A) extends FailableFunctor[ValueParser, A] {

  override def mapAttempt[B](f: A => String \/ B): ValueParser[B] =
    new ValueParser(parse(_) flatMap f)

  def flatMap[B](f: A => ValueParser[B]): ValueParser[B] =
    new ValueParser(v => parse(v).flatMap(f(_) parse v))

  def orElse(other: => ValueParser[A]): ValueParser[A] =
    new ValueParser(v => parse(v) orElse other.parse(v))
}

object ValueParser {

  def id: ValueParser[String] =
    new ValueParser(l => \/-(l.value))

  def apply[A](implicit i: ValueParser[A]): ValueParser[A] =
    i

  def apply[A](parse: String => String \/ A): ValueParser[A] =
    new ValueParser(l => parse(l.value))

  def withKey[A](parse: (Key, String) => String \/ A): ValueParser[A] =
    new ValueParser(l => parse(l.key, l.value))

  def point[A](a: => A): ValueParser[A] =
    new ValueParser[A](_ => \/-(a))

  def fail[A](reason: => String): ValueParser[A] =
    new ValueParser(_ => -\/(reason))

  implicit def scalazInstance: Monad[ValueParser] with Alternative[ValueParser] =
    new Monad[ValueParser] with Alternative[ValueParser] {
      override def point[A](a: => A)                                      = ValueParser.point(a)
      override def bind[A, B](fa: ValueParser[A])(f: A => ValueParser[B]) = fa flatMap f
      override def empty[A]                                               = ValueParser.fail("Value not specified")
      override def plus[A](a: ValueParser[A], b: => ValueParser[A])       = a orElse b
    }

  private val RegexTrue = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
  private val RegexFalse = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)

  trait Implicits {

    implicit def configValueParserString: ValueParser[String] =
      id

    implicit def configValueParserDouble: ValueParser[Double] =
      apply(_ match {
        case ParseDouble(d) => \/-(d)
        case _ => -\/("Double expected.")
      })

    implicit def configValueParserInt: ValueParser[Int] =
      apply(_ match {
        case ParseInt(i) => \/-(i)
        case _ => -\/("Int expected.")
      })

    implicit def configValueParserLong: ValueParser[Long] =
      apply(_ match {
        case ParseLong(l) => \/-(l)
        case _ => -\/("Long expected.")
      })

    implicit def configValueParserBoolean: ValueParser[Boolean] =
      apply(s =>
        if (RegexTrue.matcher(s).matches)
          \/-(true)
        else if (RegexFalse.matcher(s).matches)
          \/-(false)
        else
          -\/("Boolean expected.")
      )

    implicit def configValueParserJavaTimeChronoUnit: ValueParser[ChronoUnit] =
      id.mapOption(ParseChronoUnit.unapply)

    implicit def configValueParserJavaTimeDuration: ValueParser[Duration] =
      id.mapOption(ParseDuration.unapply)

    implicit def configValueParserJavaTimePeriod: ValueParser[Period] =
      id.mapCatch(Period.parse)

    implicit def configValueParserJavaTimeOffsetDateTime: ValueParser[OffsetDateTime] =
      id.mapCatch(OffsetDateTime.parse)

    implicit def configValueParserJavaTimeZonedDateTime: ValueParser[ZonedDateTime] =
      id.mapCatch(ZonedDateTime.parse)

    implicit def configValueParserJavaTimeLocalDateTime: ValueParser[LocalDateTime] =
      id.mapCatch(LocalDateTime.parse)

    implicit def configValueParserJavaTimeLocalDate: ValueParser[LocalDate] =
      id.mapCatch(LocalDate.parse)

    implicit def configValueParserJavaTimeLocalTime: ValueParser[LocalTime] =
      id.mapCatch(LocalTime.parse)

    implicit def configValueParserFiniteDuration(implicit d: ValueParser[Duration]): ValueParser[FiniteDuration] = {
      // Copied from https://github.com/scala/scala-java8-compat/blob/master/src/main/scala/scala/compat/java8/DurationConverters.scala
      // because it isn't published for SJS
      import java.util.concurrent.TimeUnit
      import scala.concurrent.duration.{FiniteDuration, Duration => ScalaDuration}
      def toScala(duration: java.time.Duration): scala.concurrent.duration.FiniteDuration = {
        val originalSeconds = duration.getSeconds
        val originalNanos = duration.getNano
        if (originalNanos == 0) {
          if (originalSeconds == 0) ScalaDuration.Zero
          else FiniteDuration(originalSeconds, TimeUnit.SECONDS)
        } else if (originalSeconds == 0) {
          FiniteDuration(originalNanos, TimeUnit.NANOSECONDS)
        } else {
          try {
            val secondsAsNanos = Math.multiplyExact(originalSeconds, 1000000000)
            val totalNanos = secondsAsNanos + originalNanos
            if ((totalNanos < 0 && secondsAsNanos < 0) || (totalNanos > 0 && secondsAsNanos > 0)) FiniteDuration(totalNanos, TimeUnit.NANOSECONDS)
            else throw new ArithmeticException()
          } catch {
            case _: ArithmeticException => throw new IllegalArgumentException(s"Java duration $duration cannot be expressed as a Scala duration")
          }
        }
      }
      d.map(toScala)
    }
  }

}