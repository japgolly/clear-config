package japgolly.microlibs.config

import java.time._
import java.time.temporal.ChronoUnit
import japgolly.microlibs.stdlib_ext._

object JavaTimeValueReaders extends JavaTimeValueReaders
trait JavaTimeValueReaders {

  implicit def readChronoUnit(implicit s: ValueReader[String]): ValueReader[ChronoUnit] =
    s.mapOption(ParseChronoUnit.unapply)

  implicit def readDuration(implicit s: ValueReader[String]): ValueReader[Duration] =
    s.mapOption(ParseDuration.unapply)

  implicit def readPeriod(implicit s: ValueReader[String]): ValueReader[Period] =
    s.mapCatch(Period.parse)

  implicit def readOffsetDateTime(implicit s: ValueReader[String]): ValueReader[OffsetDateTime] =
    s.mapCatch(OffsetDateTime.parse)

  implicit def readZonedDateTime(implicit s: ValueReader[String]): ValueReader[ZonedDateTime] =
    s.mapCatch(ZonedDateTime.parse)

  implicit def readLocalDateTime(implicit s: ValueReader[String]): ValueReader[LocalDateTime] =
    s.mapCatch(LocalDateTime.parse)

  implicit def readLocalDate(implicit s: ValueReader[String]): ValueReader[LocalDate] =
    s.mapCatch(LocalDate.parse)

  implicit def readLocalTime(implicit s: ValueReader[String]): ValueReader[LocalTime] =
    s.mapCatch(LocalTime.parse)

}

