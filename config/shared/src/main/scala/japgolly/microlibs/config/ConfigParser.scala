package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext._
import java.util.regex.Pattern
import scalaz.{-\/, \/, \/-}

final class ConfigParser[A](val parse: ConfigValue.Found => String \/ A) extends AnyVal {
  def map[B](f: A => B): ConfigParser[B] =
    new ConfigParser(parse(_) map f)

  def mapAttempt[B](f: A => String \/ B): ConfigParser[B] =
    new ConfigParser(parse(_) flatMap f)

  def flatMap[B](f: A => ConfigParser[B]): ConfigParser[B] =
    new ConfigParser(v => parse(v).flatMap(f(_) parse v))

  def test(errorMsg: A => Option[String]): ConfigParser[A] =
    mapAttempt(a => errorMsg(a).fold[String \/ A](\/-(a))(-\/.apply))

  def ensure(test: A => Boolean, errorMsg: => String): ConfigParser[A] =
    this.test(a => if (test(a)) None else Some(errorMsg))

  def mapCatch[B](f: A => B, e: Throwable => String = _.toString): ConfigParser[B] =
    mapAttempt(a => \/.fromTryCatchNonFatal(f(a)).leftMap(e))

  def mapOption[B](f: A => Option[B], errorMsg: => String = "Not a recognised value."): ConfigParser[B] =
    mapAttempt(f(_).fold[String \/ B](-\/(errorMsg))(\/-.apply))

  def orElse(other: => ConfigParser[A]): ConfigParser[A] =
    new ConfigParser(v => parse(v) orElse other.parse(v))
}

object ConfigParser {

  @inline def apply[A](implicit r: ConfigParser[A]) = r

  object Implicits {

    private val RegexTrue = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
    private val RegexFalse = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)

    object StringAsIs extends StringAsIs
    trait StringAsIs {
      implicit val parseString: ConfigParser[String] =
        new ConfigParser(v => \/-(v.value))
    }

    object StringDefault extends StringDefault
    trait StringDefault {
      implicit val parseString: ConfigParser[String] =
        StringAsIs.parseString.map(_.trim.replaceFirst("\\s*#.*$", ""))
    }

    object Primitives extends Primitives
    trait Primitives {
      implicit def configParserInt(implicit s: ConfigParser[String]): ConfigParser[Int] =
        s.mapAttempt {
          case ParseInt(i) => \/-(i)
          case _ => -\/("Int expected.")
        }

      implicit def configParserLong(implicit s: ConfigParser[String]): ConfigParser[Long] =
        s.mapAttempt {
          case ParseLong(l) => \/-(l)
          case _ => -\/("Long expected.")
        }

      implicit def configParserBoolean(implicit s: ConfigParser[String]): ConfigParser[Boolean] =
        s.mapAttempt(s =>
          if (RegexTrue.matcher(s).matches)
            \/-(true)
          else if (RegexFalse.matcher(s).matches)
            \/-(false)
          else
            -\/("Boolean expected.")
        )
    }

    object Defaults extends Defaults
    trait Defaults extends Primitives with StringDefault
  }
}

