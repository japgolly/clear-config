package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.Scalaz.Id
import scalaz.std.AllInstances._
import scalaz.syntax.applicative._
import utest._
import ConfigParser.Implicits.Defaults._
import Helpers._

object ConfigReportTest extends TestSuite {

  val S0 = Source.empty[Id]("S0")

  override def tests = TestSuite {

    'getNeed {
      val si: Config[Unit] =
        (Config.need[String]("s")
          |@| Config.need[Int]("i")
          |@| Config.get[Int]("no")
        )((_,_,_) => ())
      val expectUsed =
        s"""
           |+-----+-----+------+
           || Key | S1  | S2   |
           |+-----+-----+------+
           || i   | 3   | X300 |
           || no  |     |      |
           || s   | hey |      |
           |+-----+-----+------+
         """.stripMargin.trim

      val expectUnused =
        s"""
           |+-------+-------+-----+
           || Key   | S1    | S2  |
           |+-------+-------+-----+
           || dur3m | 3 min |     |
           || i2    |       | 22  |
           || in    | 100   | 200 |
           || s2    |       | ah  |
           |+-------+-------+-----+
         """.stripMargin.trim

      'used {
        "*>" - {
          val k: ConfigReport = (si *> Config.reportSoFar).run(srcs).get_!
          assertMultiline(k.reportUsed, expectUsed)
        }
        "*> <*" - {
          val k: ConfigReport = (si *> Config.reportSoFar <* Config.need[Int]("i2")).run(srcs).get_!
          assertMultiline(k.reportUsed, expectUsed)
        }
        'with {
          val (_, k: ConfigReport) = si.withReport.run(srcs).get_!
          assertMultiline(k.reportUsed, expectUsed)
        }
      }
      'unused {
        val k: ConfigReport = (si *> Config.reportSoFar).run(srcs).get_!
        assertMultiline(k.reportUnused, expectUnused)
      }

      'combined {

        'default - assertMultiline(
          si.withReport.run(srcs > S0).get_!._2.report,
          s"""
             !3 sources (highest to lowest priority):
             !  - S1
             !  - S2
             !  - S0
             !
             !Used keys (3):
             !$expectUsed
             !
             !Unused keys (4):
             !$expectUnused
           """.stripMargin('!').trim)

//        'show - println {
//          val srcs2 = Source.manual[Id]("BLAR")("user.language" -> "omg!") > srcs > Source.environment[Id] > Source.system[Id]
//          (si *> Config.get[String]("user.name") *> Config.report).run(srcs2).get_!
//            .report
//        }
      }
    }

    'getOrUse {

      'specified {
        val si = Config.need[String]("s2") tuple Config.get[String]("nope") tuple Config.getOrUse[Int]("i2", 666)
        val k = si.withReport.run(src2).get_!._2
        assertMultiline(k.report,
          s"""
             !2 sources (highest to lowest priority):
             !  - S2
             !  - API
             !
             !Used keys (3):
             !+------+----+-----+
             !| Key  | S2 | API |
             !+------+----+-----+
             !| i2   | 22 | 666 |
             !| nope |    |     |
             !| s2   | ah |     |
             !+------+----+-----+
             !
             !Unused keys (2):
             !+-----+------+
             !| Key | S2   |
             !+-----+------+
             !| i   | X300 |
             !| in  | 200  |
             !+-----+------+
             !
           """.stripMargin('!').trim)
      }

      'unspecified {
        val si = Config.need[String]("s") tuple Config.get[String]("nope") tuple Config.getOrUse[Int]("i2", 666)
        val k = si.withReport.run(src1).get_!._2
        assertMultiline(k.report,
          s"""
             !2 sources (highest to lowest priority):
             !  - S1
             !  - API
             !
             !Used keys (3):
             !+------+-----+-----+
             !| Key  | S1  | API |
             !+------+-----+-----+
             !| i2   |     | 666 |
             !| nope |     |     |
             !| s    | hey |     |
             !+------+-----+-----+
             !
             !Unused keys (3):
             !+-------+-------+
             !| Key   | S1    |
             !+-------+-------+
             !| dur3m | 3 min |
             !| i     | 3     |
             !| in    | 100   |
             !+-------+-------+
             !
           """.stripMargin('!').trim)
      }
    }

  }
}
