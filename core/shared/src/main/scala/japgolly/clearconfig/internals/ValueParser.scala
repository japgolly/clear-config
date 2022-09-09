package japgolly.clearconfig.internals

import cats.{Alternative, Monad}
import japgolly.microlibs.stdlib_ext._
import java.io.File
import java.net.{URI, URL}
import java.time._
import java.time.temporal.ChronoUnit
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex
import java.net.InetAddress
import java.util.UUID

final class ValueParser[A](val parse: String => Either[String, A]) extends FailableFunctor[ValueParser, A] {

  override def mapAttempt[B](f: A => Either[String, B]): ValueParser[B] =
    new ValueParser(parse(_) flatMap f)

  def flatMap[B](f: A => ValueParser[B]): ValueParser[B] =
    new ValueParser(v => parse(v).flatMap(f(_) parse v))

  def orElse(other: => ValueParser[A]): ValueParser[A] =
    new ValueParser(v => parse(v) orElse other.parse(v))

  def preprocessValue(f: String => String): ValueParser[A] =
    new ValueParser(v => parse(f(v)))
}

object ValueParser {

  def id: ValueParser[String] =
    new ValueParser(Right(_))

  def apply[A](implicit i: ValueParser[A]): ValueParser[A] =
    i

  def apply[A](parse: String => Either[String, A]): ValueParser[A] =
    new ValueParser(parse)

  def pure[A](a: A): ValueParser[A] =
    new ValueParser[A](_ => Right(a))

  def point[A](a: => A): ValueParser[A] =
    new ValueParser[A](_ => Right(a))

  def fail[A](reason: => String): ValueParser[A] =
    new ValueParser(_ => Left(reason))

  def oneOf[A](values: (String, A)*): ValueParser[A] = {
    var m = Map.empty[String, A]
    for ((k, v) <- values) {
      for (v2 <- m.get(k))
        throw new IllegalArgumentException(s"Config value '$k' has multiple values: $v and $v2")
      m = m.updated(k, v)
    }
    id.mapOption(m.get, m.keys.toList.sorted.mkString("Legal values are: ", ", ", "."))
  }

  implicit def catsInstance: Monad[ValueParser] with Alternative[ValueParser] =
    new Monad[ValueParser] with Alternative[ValueParser] {
      override def pure[A](a: A)                                             = ValueParser.pure(a)
      override def flatMap[A, B](fa: ValueParser[A])(f: A => ValueParser[B]) = fa.flatMap(f)
      override def empty[A]                                                  = ValueParser.fail("Value not specified")
      override def combineK[A](a: ValueParser[A], b: ValueParser[A])         = a orElse b

      override def tailRecM[A, B](z: A)(f: A => ValueParser[Either[A, B]]): ValueParser[B] =
        new ValueParser[B](s => {
          @tailrec
          def go(a: A): Either[String, B] =
            f(a).parse(s) match {
              case Right(Left(a))  => go(a)
              case Right(Right(b)) => Right(b)
              case Left(e)         => Left(e)
            }
          go(z)
        })
    }

  private val RegexTrue = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
  private val RegexFalse = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)

  trait Implicits {

    implicit def configValueParserString: ValueParser[String] =
      id

    implicit def configValueParserDouble: ValueParser[Double] =
      apply(_ match {
        case ParseDouble(d) => Right(d)
        case _ => Left("Double expected.")
      })

    implicit def configValueParserInt: ValueParser[Int] =
      apply(_ match {
        case ParseInt(i) => Right(i)
        case _ => Left("Int expected.")
      })

    implicit def configValueParserLong: ValueParser[Long] =
      apply(_ match {
        case ParseLong(l) => Right(l)
        case _ => Left("Long expected.")
      })

    implicit def configValueParserBoolean: ValueParser[Boolean] =
      apply(s =>
        if (RegexTrue.matcher(s).matches)
          Right(true)
        else if (RegexFalse.matcher(s).matches)
          Right(false)
        else
          Left("Boolean expected.")
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

    implicit def configValueParserURI: ValueParser[URI] =
      id.mapCatch(new URI(_))

    implicit def configValueParserURL: ValueParser[URL] =
      id.mapCatch(new URL(_))

    implicit def configValueParserFile: ValueParser[File] =
      id.mapCatch(new File(_))

    implicit def configValueParserRegex: ValueParser[Regex] =
      id.mapCatch(_.r)

    implicit def configValueParserPattern: ValueParser[Pattern] =
      configValueParserRegex.map(_.pattern)

    implicit def configValueParserInetAddress: ValueParser[InetAddress] =
      id.mapCatch(InetAddress.getByName)

    implicit def configValueParserUUID: ValueParser[UUID] =
      id.mapCatch(UUID.fromString)
  }

}
