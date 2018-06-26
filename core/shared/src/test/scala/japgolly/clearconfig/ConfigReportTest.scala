package japgolly.clearconfig

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.testutil.TestUtil._
import scalaz.Scalaz.Id
import scalaz.syntax.applicative._
import utest._
import Helpers._

object ConfigReportTest extends TestSuite {

  val S0 = ConfigSource.empty[Id]("S0")

  override def tests = Tests {

    'getNeed {
      val si: ConfigDef[Unit] =
        (ConfigDef.need[String]("s")
          |@| ConfigDef.need[Int]("i")
          |@| ConfigDef.get[Int]("no")
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
//        "*>" - {
//          val k: ConfigReport = (si *> ConfigDef.reportSoFar).run(srcs).get_!
//          assertMultiline(k.reportUsed, expectUsed)
//        }
//        "*> <*" - {
//          val k: ConfigReport = (si *> ConfigDef.reportSoFar <* ConfigDef.need[Int]("i2")).run(srcs).get_!
//          assertMultiline(k.reportUsed, expectUsed)
//        }
        'with {
          val (_, k: ConfigReport) = si.withReport.run(srcs).get_!
          assertMultiline(k.reportUsed, expectUsed)
        }
      }
      'unused {
        val k: ConfigReport = si.withReport.map(_._2).run(srcs).get_!
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
//          val srcs2 = ConfigSource.manual[Id]("BLAR")("user.language" -> "omg!") > srcs > ConfigSource.environment[Id] > ConfigSource.system[Id]
//          (si *> ConfigDef.get[String]("user.name") *> ConfigDef.report).run(srcs2).get_!
//            .report
//        }
      }
    }

    'getOrUse {

      'specified {
        val si = ConfigDef.need[String]("s2") tuple ConfigDef.get[String]("nope") tuple ConfigDef.getOrUse[Int]("i2", 666)
        val k = si.withReport.run(src2).get_!._2
        assertMultiline(k.report,
          s"""
             !2 sources (highest to lowest priority):
             !  - S2
             !  - Default
             !
             !Used keys (3):
             !+------+----+---------+
             !| Key  | S2 | Default |
             !+------+----+---------+
             !| i2   | 22 | 666     |
             !| nope |    |         |
             !| s2   | ah |         |
             !+------+----+---------+
             !
             !Unused keys (2):
             !+-----+------+
             !| Key | S2   |
             !+-----+------+
             !| i   | X300 |
             !| in  | 200  |
             !+-----+------+
           """.stripMargin('!').trim)
      }

      'unspecified {
        val si = ConfigDef.need[String]("s") tuple ConfigDef.get[String]("nope") tuple ConfigDef.getOrUse[Int]("i2", 666)
        val k = si.withReport.run(src1).get_!._2
        assertMultiline(k.report,
          s"""
             !2 sources (highest to lowest priority):
             !  - S1
             !  - Default
             !
             !Used keys (3):
             !+------+-----+---------+
             !| Key  | S1  | Default |
             !+------+-----+---------+
             !| i2   |     | 666     |
             !| nope |     |         |
             !| s    | hey |         |
             !+------+-----+---------+
             !
             !Unused keys (3):
             !+-------+-------+
             !| Key   | S1    |
             !+-------+-------+
             !| dur3m | 3 min |
             !| i     | 3     |
             !| in    | 100   |
             !+-------+-------+
           """.stripMargin('!').trim)
      }
    }

    'mapKeyQueries {
      'oneSource {
        val s = ConfigSource.manual[Id]("S")(
          "both.1" -> "YAY", "both_1" -> "NOPE",
          "db_port" -> "1234")
          .mapKeyQueries(k => List(k, k.replace('.', '_')))
        val c = ConfigDef.need[Int]("db.port") tuple ConfigDef.need[String]("both.1")
        val r = c.withReport.run(s).get_!._2
        assertMultiline(r.report,
          s"""
             !1 source:
             !  - S
             !
             !Used keys (3):
             !+---------+------+
             !| Key     | S    |
             !+---------+------+
             !| both.1  | YAY  |
             !| db.port |      |
             !| db_port | 1234 |
             !+---------+------+
             !
             !Unused keys (1):
             !+--------+------+
             !| Key    | S    |
             !+--------+------+
             !| both_1 | NOPE |
             !+--------+------+
           """.stripMargin('!').trim)
      }
      'multipleSources {
        val s1 = ConfigSource.manual[Id]("S1")(
          "both.1" -> "YAY", "both_1" -> "NOPE",
          "db_port" -> "1234")
          .mapKeyQueries(k => List(k, k.replace('.', '_')))
        val s2 = ConfigSource.manual[Id]("S2")("db_port" -> "9875").mapKeyQueries(k => List(k, k.replace('.', '_')))
        val s3 = ConfigSource.manual[Id]("S3")("db_port" -> "3333")
        val s = s1 > s2 > s3
        val c = ConfigDef.need[Int]("db.port") tuple ConfigDef.need[String]("both.1")
        val r = c.withReport.run(s).get_!._2
        assertMultiline(r.report,
          s"""
             !3 sources (highest to lowest priority):
             !  - S1
             !  - S2
             !  - S3
             !
             !Used keys (3):
             !+---------+------+------+
             !| Key     | S1   | S2   |
             !+---------+------+------+
             !| both.1  | YAY  |      |
             !| db.port |      |      |
             !| db_port | 1234 | 9875 |
             !+---------+------+------+
             !
             !Unused keys (2):
             !+---------+------+------+
             !| Key     | S1   | S3   |
             !+---------+------+------+
             !| both_1  | NOPE |      |
             !| db_port |      | 3333 |
             !+---------+------+------+
             !
           """.stripMargin('!').trim)
      }
    }

    'choice {
      val ci = ConfigDef.need[Int]("C").ensure(1.to(2).contains, "Choose 1 or 2")
      val c1 = ConfigDef.need[String]("C1")
      val c2 = ConfigDef.need[String]("C2")
      val cc = ci.choose(i => if (i == 1) c1 else c2)
      val v1 = "see one"
      val v2 = "sea too"
      val s = ConfigSource.manual[Id]("S")("C" -> "1", "C1" -> v1, "C2" -> v2)

      val r = cc.withReport.run(s).get_!._2
      assertMultiline(r.report,
        s"""
           !1 source:
           !  - S
           !
           !Used keys (2):
           !+-----+---------+
           !| Key | S       |
           !+-----+---------+
           !| C   | 1       |
           !| C1  | see one |
           !+-----+---------+
           !
           !Unused keys (1):
           !+-----+---------+
           !| Key | S       |
           !+-----+---------+
           !| C2  | sea too |
           !+-----+---------+
           !
         """.stripMargin('!').trim)
    }

    'secret {
      val a = ConfigDef.need[String]("a")
      val b = ConfigDef.need[String]("b").secret
      val c = ConfigDef.need[String]("c")
      val s = ConfigSource.manual[Id]("S")("a" -> "1", "b" -> "2", "c" -> "3")
      val r = (a tuple b tuple c withReport).run(s).get_!._2
      assertMultiline(r.report.removeAnsiEscapeCodes,
        s"""
           !1 source:
           !  - S
           !
           !Used keys (3):
           !+-----+----------------+
           !| Key | S              |
           !+-----+----------------+
           !| a   | 1              |
           !| b   | <# 16CC649D #> |
           !| c   | 3              |
           !+-----+----------------+
           !
           !Unused keys (0):
           !No data to report.
         """.stripMargin('!').trim)
    }

    'filtering {
      val s1 = ConfigSource.manual[Id]("S1")("a" -> "a1", "s" -> "s1")
      val s2 = ConfigSource.manual[Id]("S2")("b" -> "b2", "s" -> "s2")
      val s3 = ConfigSource.manual[Id]("S3")()
      val s = s1 > s2 > s3
      val c = ConfigDef.const(1)

      val r = c.withReport.run(s).get_!._2

      'default - assertMultiline(r.report,
        s"""
           !3 sources (highest to lowest priority):
           !  - S1
           !  - S2
           !  - S3
           !
           !Used keys (0):
           !No data to report.
           !
           !Unused keys (3):
           !+-----+----+----+
           !| Key | S1 | S2 |
           !+-----+----+----+
           !| a   | a1 |    |
           !| b   |    | b2 |
           !| s   | s1 | s2 |
           !+-----+----+----+
         """.stripMargin('!').trim)

      'withoutS1 - assertMultiline(r.mapUnused(_.withoutSources(s1.name)).reportUnused,
        s"""
           !+-----+----+
           !| Key | S2 |
           !+-----+----+
           !| b   | b2 |
           !| s   | s2 |
           !+-----+----+
         """.stripMargin('!').trim)

      'withoutS12 - assertMultiline(r.mapUnused(_.withoutSources(s1.name, s2.name)).reportUnused,
        s"""
           !No data to report.
         """.stripMargin('!').trim)

    }
  }
}
