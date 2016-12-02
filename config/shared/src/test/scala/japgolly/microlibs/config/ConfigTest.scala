package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.{-\/, \/-}
import scalaz.std.AllInstances._
import scalaz.syntax.applicative._
import scalaz.Scalaz.Id
import utest._
import ValueReader.Implicits.Defaults._

object ConfigTest extends TestSuite {

  implicit def equalResultX[A] = scalaz.Equal.equalA[ConfigResult[A]]

  val src1 = Source.manual[Id]("S1")("in" -> "100", "i" -> "3", "s" -> "hey", "dur3m" -> "3 min")
  val src2 = Source.manual[Id]("S2")("in" -> "200", "i" -> "X300", "i2" -> "22", "s2" -> "ah")

  val srcs: Sources[Id] =
     src1 > src2

  val srcE = Source.point[Id]("SE", new ConfigStore[Id] {
    override def apply(key: Key) = ConfigValue.Error("This source is fake!", None)
    override def getBulk(f: Key => Boolean) = Map.empty
  })

  implicit class ResultXExt[A](private val self: ConfigResult[A]) extends AnyVal {
    def get_! : A = self match {
      case ConfigResult.Success(a) => a
      case x => fail(s"Expected success, got: $x")
    }
  }

  override def tests = TestSuite {

    'findFirst -
      assertEq(Config.need[String]("s").run(srcs), ConfigResult.Success("hey"))

    'findSecond -
      assertEq(Config.need[Int]("i2").run(srcs), ConfigResult.Success(22))

    'notFound -
      assertEq(Config.get[Int]("notfound").run(srcs), ConfigResult.Success(Option.empty[Int]))

    'missing1 -
      assertEq(Config.need[Int]("missing").run(srcs), ConfigResult.QueryFailure(Map(Key("missing") -> None)))

    'missing2 -
      assertEq(
        (Config.need[Int]("no1") tuple Config.need[Int]("no2")).run(srcs),
        ConfigResult.QueryFailure(Map(Key("no1") -> None, Key("no2") -> None)))

    'valueFail1 -
      assertEq(
        Config.need[Int]("s").run(srcs),
        ConfigResult.QueryFailure(Map(Key("s") -> Some((src1.name, ConfigValue.Error("Int expected.", Some("hey")))))))

    'valueFail2 -
      assertEq(
        Config.need[Int]("s2").run(srcs),
        ConfigResult.QueryFailure(Map(Key("s2") -> Some((src2.name, ConfigValue.Error("Int expected.", Some("ah")))))))

    'errorMsg {
      'notFound - assertEq(Config.need[Int]("QQ").run(srcs).toDisjunction, -\/(
        """
          |1 error:
          |  - No value for key [QQ]
        """.stripMargin.trim))

      'notFound2 - {
        val c = Config.need[Int]("QQ") tuple Config.get[Int]("X") tuple Config.need[Int]("i") tuple Config.need[Int]("M")
        assertEq(c.run(srcs).toDisjunction, -\/(
          """
            |2 errors:
            |  - No value for key [M]
            |  - No value for key [QQ]
          """.stripMargin.trim))
      }

      'errors2 - {
        val c = Config.need[Int]("s") tuple Config.get[Int]("X")
        assertEq(c.run(srcs > srcE).toDisjunction, -\/(
          """
            |2 errors:
            |  - Error reading key [X] from source [SE]: This source is fake!
            |  - Error reading key [s] from source [S1] with value [hey]: Int expected.
          """.stripMargin.trim))
      }
    }

    'ensure {
      'ok - assertEq(
        Config.need("in")(ValueReader[Int].ensure(_ < 150, "Must be < 150.")).run(srcs),
        ConfigResult.Success(100))

      'ko - assertEq(
        Config.need("in")(ValueReader[Int].ensure(_ > 150, "Must be > 150.")).run(srcs),
        ConfigResult.QueryFailure(Map(Key("in") -> Some((src1.name, ConfigValue.Error("Must be > 150.", Some("100")))))))
    }

    'report {
      val si: Config[Unit] = (Config.need[String]("s") |@| Config.need[Int]("i") |@| Config.get[Int]("no"))((_,_,_) => ())
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
          val k: Report = (si *> Config.keyReport).run(srcs).get_!
          assertEq(k.reportUsed, expectUsed)
        }
        "*> <*" - {
          val k: Report = (si *> Config.keyReport <* Config.need[Int]("i2")).run(srcs).get_!
          assertEq(k.reportUsed, expectUsed)
        }
        'with {
          val (_, k: Report) = si.withReport.run(srcs).get_!
          assertEq(k.reportUsed, expectUsed)
        }
      }
      'unused {
        val k: Report = (si *> Config.keyReport).run(srcs).get_!
        assertEq(k.reportUnused, expectUnused)
      }
//      'combined - (si *> Config.keyReport).run(srcs).get_!.report
      'combined - println {
        // AWS_SECRET_KEY
        // *password*
        val srcs2 = Source.manual[Id]("BLAR")("user.language" -> "omg!") > srcs > Source.environment[Id] > Source.system[Id]
        (si *> Config.get[String]("user.name") *> Config.keyReport).run(srcs2).get_!
          .report
      }
    }

    'keyMod {
      'prefix {
        val s = Source.manual[Id]("S")(
          "a.b.1" -> "AB-1", "a.1" -> "A!", "b.1" -> "B!", "1" -> "I",
          "a.b.2" -> "AB-2", "a.2" -> "A@", "b.2" -> "B@", "2" -> "II")
        val one = Config.need[String]("1")
        val two = Config.need[String]("2")

        * - assertEq((one tuple two.withPrefix("b.")).run(s).get_!, ("I", "B@"))
        * - assertEq((one.withPrefix("b.") tuple two).run(s).get_!, ("B!", "II"))
        * - assertEq((one tuple two).withPrefix("b.").run(s).get_!, ("B!", "B@"))
        * - assertEq((one tuple two.withPrefix("b.")).withPrefix("a.").run(s).get_!, ("A!", "AB-2"))

        'missing - assertEq(
          one.withPrefix("omg.").run(s),
          ConfigResult.QueryFailure(Map(Key("omg.1") -> None)))
      }

      'caseInsensitive {
        val c = Config.get[String]("S2")
        assertEq((c tuple c.withCaseInsensitiveKeys).run(srcs).get_!, (None, Some("ah")))
      }
    }

  }
}
