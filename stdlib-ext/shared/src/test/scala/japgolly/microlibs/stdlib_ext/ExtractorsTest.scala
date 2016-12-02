package japgolly.microlibs.stdlib_ext

import japgolly.microlibs.testutil.TestUtil._
import java.time.Duration
import utest._

object ExtractorsTest extends TestSuite {
  implicit def equalDammit[A]: scalaz.Equal[A] = scalaz.Equal.equalA[A]

  override def tests = TestSuite {

    'duration {
      "2d" - assertEq(ParseDuration.unapply("2d"), Option(Duration.ofDays(2)))
      "2d9s" - assertEq(ParseDuration.unapply("2d9s"), Option(Duration.ofDays(2) plus Duration.ofSeconds(9)))
      "2D9S" - assertEq(ParseDuration.unapply("2D9S"), Option(Duration.ofDays(2) plus Duration.ofSeconds(9)))
      "2 days, 8 minutes" - assertEq(ParseDuration.unapply("2 days, 8 minutes"), Option(Duration.ofDays(2) plus Duration.ofMinutes(8)))
      "2 DAYS, 8 MINUTES" - assertEq(ParseDuration.unapply("2 DAYS, 8 MINUTES"), Option(Duration.ofDays(2) plus Duration.ofMinutes(8)))
    }

  }
}
