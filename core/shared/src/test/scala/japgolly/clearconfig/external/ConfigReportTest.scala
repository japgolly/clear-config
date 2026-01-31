package japgolly.clearconfig.external

import cats.Id
import cats.instances.string._
import cats.syntax.all._
import japgolly.clearconfig.Helpers._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.testutil.TestUtil._
import utest._

object ConfigReportTest extends TestSuite {

  val S0 = ConfigSource.empty[Id]("S0")

  override def tests = Tests {

    "getNeed" - {
      val si: ConfigDef[Unit] =
        (ConfigDef.need[String]("s"),
          ConfigDef.need[Int]("i"),
          ConfigDef.get[Int]("no")
        ).tupled.void
      val expectUsed =
        s"""
           |Used keys (3):
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
           |Unused keys (4):
           |+-------+-------+-----+
           || Key   | S1    | S2  |
           |+-------+-------+-----+
           || dur3m | 3 min |     |
           || i2    |       | 22  |
           || in    | 100   | 200 |
           || s2    |       | ah  |
           |+-------+-------+-----+
         """.stripMargin.trim

      "used" - {
//        "*>" - {
//          val k: ConfigReport = (si *> ConfigDef.reportSoFar).run(srcs).get_!
//          assertMultiline(k.reportUsed, expectUsed)
//        }
//        "*> <*" - {
//          val k: ConfigReport = (si *> ConfigDef.reportSoFar <* ConfigDef.need[Int]("i2")).run(srcs).get_!
//          assertMultiline(k.reportUsed, expectUsed)
//        }
        "with" - {
          val (_, k: ConfigReport) = si.withReport.run(srcs).get_!
          assertMultiline(k.used, expectUsed)
        }
      }
      "unused" - {
        val k: ConfigReport = si.withReport.map(_._2).run(srcs).get_!
        assertMultiline(k.unused, expectUnused)
      }

      "combined" - {

        "default" - assertMultiline(
          si.withReport.run(srcs > S0).get_!._2.full,
          s"""
             !3 sources (highest to lowest priority):
             !  - S1
             !  - S2
             !  - S0
             !
             !$expectUsed
             !
             !$expectUnused
           """.stripMargin('!').trim)

//        'show - println {
//          val srcs2 = ConfigSource.manual[Id]("BLAR")("user.language" -> "omg!") > srcs > ConfigSource.environment[Id] > ConfigSource.system[Id]
//          (si *> ConfigDef.get[String]("user.name") *> ConfigDef.report).run(srcs2).get_!
//            .report
//        }
      }
    }

    "getOrUse" - {

      "specified" - {
        val si = (
          ConfigDef.need[String]("s2"),
          ConfigDef.get[String]("nope"),
          ConfigDef.getOrUse[Int]("i2", 666)
        ).tupled
        val k = si.withReport.run(src2).get_!._2
        assertMultiline(k.full,
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

      "unspecified" - {
        val si = (
          ConfigDef.need[String]("s"),
          ConfigDef.get[String]("nope"),
          ConfigDef.getOrUse[Int]("i2", 666)
        ).tupled
        val k = si.withReport.run(src1).get_!._2
        assertMultiline(k.full,
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

      "null" - {
        val c = ConfigDef.getOrUse[String]("x", null)
        val k = c.withReport.run(src0).get_!._2
        assertMultiline(k.full,
          s"""
             !2 sources (highest to lowest priority):
             !  - S0
             !  - Default
             !
             !Used keys (1):
             !+-----+---------+
             !| Key | Default |
             !+-----+---------+
             !| x   | null    |
             !+-----+---------+
             !
             !Unused keys (0):
             !No data to report.
           """.stripMargin('!').trim)
      }
    }

    "blankValues" - {
      implicit val configValuePreprocessor = ConfigValuePreprocessor.id
      val c = (ConfigDef.need[String]("a"), ConfigDef.need[String]("b")).tupled.withReport.map(_._2)
      val s = ConfigSource.manual[Id]("S")("a" -> "", "b" -> " \t ")
      val r = c.run(s).get_!
      assertMultiline(
        """
          |Used keys (2):
          |+-----+--------+
          || Key | S      |
          |+-----+--------+
          || a   | ""     |
          || b   | " \t " |
          |+-----+--------+
        """.stripMargin.trim, r.used)
    }

    "mapKeyQueries" - {
      "oneSource" - {
        val s = ConfigSource.manual[Id]("S")(
          "both.1" -> "YAY", "both_1" -> "NOPE",
          "db_port" -> "1234")
          .mapKeyQueries(k => List(k, k.replace('.', '_')))
        val c = (ConfigDef.need[Int]("db.port"), ConfigDef.need[String]("both.1")).tupled
        val r = c.withReport.run(s).get_!._2
        assertMultiline(r.full,
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
      "multipleSources" - {
        val s1 = ConfigSource.manual[Id]("S1")(
          "both.1" -> "YAY", "both_1" -> "NOPE",
          "db_port" -> "1234")
          .mapKeyQueries(k => List(k, k.replace('.', '_')))
        val s2 = ConfigSource.manual[Id]("S2")("db_port" -> "9875").mapKeyQueries(k => List(k, k.replace('.', '_')))
        val s3 = ConfigSource.manual[Id]("S3")("db_port" -> "3333")
        val s = s1 > s2 > s3
        val c = (ConfigDef.need[Int]("db.port"), ConfigDef.need[String]("both.1")).tupled
        val r = c.withReport.run(s).get_!._2
        assertMultiline(r.full,
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

    "choice" - {
      val ci = ConfigDef.need[Int]("C").ensure(1.to(2).contains, "Choose 1 or 2")
      val c1 = ConfigDef.need[String]("C1")
      val c2 = ConfigDef.need[String]("C2")
      val cc = ci.choose(i => if (i == 1) c1 else c2)
      val v1 = "see one"
      val v2 = "sea too"
      val s = ConfigSource.manual[Id]("S")("C" -> "1", "C1" -> v1, "C2" -> v2)

      val r = cc.withReport.run(s).get_!._2
      assertMultiline(r.full,
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

    "secret" - {
      val a = ConfigDef.need[String]("a")
      val b = ConfigDef.need[String]("b").secret
      val c = ConfigDef.need[String]("c")
      val s = ConfigSource.manual[Id]("S")("a" -> "1", "b" -> "2", "c" -> "3")
      val r = (a, b, c).tupled.withReport.run(s).get_!._2
      assertMultiline(r.full.removeAnsiEscapeCodes,
        s"""
           !1 source:
           !  - S
           !
           !Used keys (3):
           !+-----+-----------------------+
           !| Key | S                     |
           !+-----+-----------------------+
           !| a   | 1                     |
           !| b   | Obfuscated (16CC649D) |
           !| c   | 3                     |
           !+-----+-----------------------+
           !
           !Unused keys (0):
           !No data to report.
         """.stripMargin('!').trim)
    }

    "filtering" - {
      val s1 = ConfigSource.manual[Id]("S1")("a" -> "a1", "s" -> "s1")
      val s2 = ConfigSource.manual[Id]("S2")("b" -> "b2", "s" -> "s2")
      val s3 = ConfigSource.manual[Id]("S3")()
      val s = s1 > s2 > s3
      val c = ConfigDef.const(1)

      val r = c.withReport.run(s).get_!._2

      "default" - assertMultiline(r.full,
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

      "withoutS1" - assertMultiline(r.mapUnused(_.withoutSources(s1.name)).unused,
        s"""
           !Unused keys (2):
           !+-----+----+
           !| Key | S2 |
           !+-----+----+
           !| b   | b2 |
           !| s   | s2 |
           !+-----+----+
         """.stripMargin('!').trim)

      "withoutS12" - assertMultiline(r.mapUnused(_.withoutSources(s1.name, s2.name)).unused,
        s"""
           !Unused keys (0):
           !No data to report.
         """.stripMargin('!').trim)

       "filterKeysAndSourceNot" - {
        val r2 = r.mapUnused(_.filterKeysAndSourceNot(_.startsWith("s") && _ == s1.name))
        assertMultiline(r2.unused,
          s"""
             !Unused keys (3):
             !+-----+----+----+
             !| Key | S1 | S2 |
             !+-----+----+----+
             !| a   | a1 |    |
             !| b   |    | b2 |
             !| s   |    | s2 |
             !+-----+----+----+
           """.stripMargin('!').trim)
       }

       "filterKeysNotWithSources" - {
        val r2 = r.mapUnused(_.filterKeysNot(_.startsWith("s"), s1.name))
        assertMultiline(r2.unused,
          s"""
             !Unused keys (3):
             !+-----+----+----+
             !| Key | S1 | S2 |
             !+-----+----+----+
             !| a   | a1 |    |
             !| b   |    | b2 |
             !| s   |    | s2 |
             !+-----+----+----+
           """.stripMargin('!').trim)
       }

       "filterKeysWithSources1" - {
        val r2 = r.mapUnused(_.filterKeys(_.startsWith("s"), s1.name))
        assertMultiline(r2.unused,
          s"""
             !Unused keys (2):
             !+-----+----+----+
             !| Key | S1 | S2 |
             !+-----+----+----+
             !| b   |    | b2 |
             !| s   | s1 | s2 |
             !+-----+----+----+
           """.stripMargin('!').trim)
       }

       "filterKeysWithSources2" - {
        val r2 = r.mapUnused(_.filterKeys(_.startsWith("s"), s1.name, s2.name))
        assertMultiline(r2.unused,
          s"""
             !Unused keys (1):
             !+-----+----+----+
             !| Key | S1 | S2 |
             !+-----+----+----+
             !| s   | s1 | s2 |
             !+-----+----+----+
           """.stripMargin('!').trim)
       }
    }

    "colour" - {
      val c = ConfigDef.need[Int]("i").secret
      val s = ConfigSource.manual[Id]("S")("i" -> "1")
      val r = c.withReport.run(s).get_!._2
      "on" - assertMultiline(r.withColour.used(false),
        s"""
          |+-----+-----------------------+
          || Key | S                     |
          |+-----+-----------------------+
          || i   | ${Console.YELLOW}Obfuscated (9C554F15)${Console.RESET} |
          |+-----+-----------------------+
        """.stripMargin.trim)
      "off" - assertMultiline(r.withoutColour.used(false),
        """
          |+-----+-----------------------+
          || Key | S                     |
          |+-----+-----------------------+
          || i   | Obfuscated (9C554F15) |
          |+-----+-----------------------+
        """.stripMargin.trim)
    }

    "caseInsensitiveSource" - {
      val s = ConfigSource.manual[Id]("S")("a" -> "no", "Ab" -> "@", "p" -> "no").caseInsensitive
      val c = ConfigDef.need[String]("ab")
      val (a, r) = c.withReport.run(s).getOrDie()
      assertEq(a, "@")
      assertMultiline(r.full,
        """
          |1 source:
          |  - S
          |
          |Used keys (2):
          |+-----+---+
          || Key | S |
          |+-----+---+
          || Ab  | @ |
          || ab  |   |
          |+-----+---+
          |
          |Unused keys (2):
          |+-----+----+
          || Key | S  |
          |+-----+----+
          || a   | no |
          || p   | no |
          |+-----+----+
        """.stripMargin.trim)
    }
  }
}
