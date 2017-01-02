package japgolly.microlibs.config

import java.time._
import java.time.temporal.ChronoUnit
import japgolly.microlibs.stdlib_ext._

object JavaTimeValueReaders extends JavaTimeValueReaders
trait JavaTimeValueReaders {

  implicit def readChronoUnit(implicit s: ConfigParser[String]): ConfigParser[ChronoUnit] =
    s.mapOption(ParseChronoUnit.unapply)

  implicit def readDuration(implicit s: ConfigParser[String]): ConfigParser[Duration] =
    s.mapOption(ParseDuration.unapply)

  implicit def readPeriod(implicit s: ConfigParser[String]): ConfigParser[Period] =
    s.mapCatch(Period.parse)

  implicit def readOffsetDateTime(implicit s: ConfigParser[String]): ConfigParser[OffsetDateTime] =
    s.mapCatch(OffsetDateTime.parse)

  implicit def readZonedDateTime(implicit s: ConfigParser[String]): ConfigParser[ZonedDateTime] =
    s.mapCatch(ZonedDateTime.parse)

  implicit def readLocalDateTime(implicit s: ConfigParser[String]): ConfigParser[LocalDateTime] =
    s.mapCatch(LocalDateTime.parse)

  implicit def readLocalDate(implicit s: ConfigParser[String]): ConfigParser[LocalDate] =
    s.mapCatch(LocalDate.parse)

  implicit def readLocalTime(implicit s: ConfigParser[String]): ConfigParser[LocalTime] =
    s.mapCatch(LocalTime.parse)

}

