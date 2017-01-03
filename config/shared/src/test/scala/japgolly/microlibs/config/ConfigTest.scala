package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.{-\/, \/-}
import scalaz.Scalaz.Id
import scalaz.std.AllInstances._
import scalaz.syntax.applicative._
import utest._
import ConfigParser.Implicits.Defaults._
import Helpers._

object ConfigTest extends TestSuite {

  override def tests = TestSuite {

    'findFirst -
      assertEq(Config.need[String]("s").run(srcs), ConfigResult.Success("hey"))

    'findSecond -
      assertEq(Config.need[Int]("i2").run(srcs), ConfigResult.Success(22))

    'notFound -
      assertEq(Config.get[Int]("notfound").run(srcs), ConfigResult.Success(Option.empty[Int]))

    'missing1 -
      assertEq(Config.need[Int]("missing").run(srcs), ConfigResult.QueryFailure(Map(Key("missing") -> None), Set.empty, srcNames))

    'missing2 -
      assertEq(
        (Config.need[Int]("no1") tuple Config.need[Int]("no2")).run(srcs),
        ConfigResult.QueryFailure(Map(Key("no1") -> None, Key("no2") -> None), Set.empty, srcNames))

    'valueFail1 -
      assertEq(
        Config.need[Int]("s").run(srcs),
        ConfigResult.QueryFailure(Map(Key("s") -> Some((src1.name, ConfigValue.Error("Int expected.", Some("hey"))))), Set.empty, srcNames))

    'valueFail2 -
      assertEq(
        Config.need[Int]("s2").run(srcs),
        ConfigResult.QueryFailure(Map(Key("s2") -> Some((src2.name, ConfigValue.Error("Int expected.", Some("ah"))))), Set.empty, srcNames))

    'errorMsg {
      'notFound - assertEq(Config.need[Int]("QQ").run(srcs).toDisjunction, -\/(
        """
          |1 error:
          |  - No value for key [QQ]
          |
          |2 sources:
          |  - S1
          |  - S2
        """.stripMargin.trim))

      'notFound2 - {
        val c = Config.need[Int]("QQ") tuple Config.get[Int]("X") tuple Config.need[Int]("i") tuple Config.need[Int]("M")
        assertEq(c.run(srcs).toDisjunction, -\/(
          """
            |2 errors:
            |  - No value for key [M]
            |  - No value for key [QQ]
            |
            |2 sources:
            |  - S1
            |  - S2
          """.stripMargin.trim))
      }

      'errors2 - {
        val c = Config.need[Int]("s") tuple Config.get[Int]("X")
        assertEq(c.run(srcs > srcE).toDisjunction, -\/(
          """
            |2 errors:
            |  - Error reading key [X] from source [SE]: This source is fake!
            |  - Error reading key [s] from source [S1] with value [hey]: Int expected.
            |
            |3 sources:
            |  - S1
            |  - S2
            |  - SE
          """.stripMargin.trim))
      }

      'unkeyedErrors {
        val c1 = Config.need[Int]("in").map(_ + 1000).ensure(_ > 1150, "Must be > 1150.")
        val c2 = 7.point[Config].ensure(_ > 10, "Must be > 10.")
        val c3 = (Config.need[Int]("in") |@| Config.need[Int]("i2"))(_ + _).ensure(_ > 150, "Must be > 150.")
        val c = c1 tuple c2 tuple c3
        assertEq(c.run(srcs > srcE).toDisjunction, -\/(
          """
            |3 errors:
            |  - Error using <function>, key [i2], key [in]: Must be > 150.
            |  - Error using <function>, key [in]: Must be > 1150.
            |  - Error using runtime value [7]: Must be > 10.
            |
            |3 sources:
            |  - S1
            |  - S2
            |  - SE
          """.stripMargin.trim))
      }
    }

    'ensure {
      'read1 {
        val c = Config.need[Int]("in")
        'ok - assertEq(
          c.ensure(_ < 150, "Must be < 150.").run(srcs),
          ConfigResult.Success(100))
        'ko - assertEq(
          c.ensure(_ > 150, "Must be > 150.").run(srcs),
          ConfigResult.QueryFailure(Map(Key("in") -> Some((src1.name, ConfigValue.Error("Must be > 150.", Some("100"))))), Set.empty, srcNames))
      }

      'readMap1 {
        val c = Config.need[Int]("in").map(_ + 1000)
        'ok - assertEq(
          c.ensure(_ > 1050, "Must be > 1050.").run(srcs),
          ConfigResult.Success(1100))
        'ko - assertEq(
          c.ensure(_ > 1150, "Must be > 1150.").run(srcs),
          ConfigResult.QueryFailure(Map.empty, Set("Must be > 1150." -> Set(\/-(Key("in")), -\/("<function>"))), srcNames))
      }

      'read2 {
        val c = (Config.need[Int]("in") |@| Config.need[Int]("i2"))(_ + _)
        'ok - assertEq(
          c.ensure(_ < 150, "Must be < 150.").run(srcs),
          ConfigResult.Success(122))
        'ko - assertEq(
          c.ensure(_ > 150, "Must be > 150.").run(srcs),
          ConfigResult.QueryFailure(Map.empty, Set("Must be > 150." -> Set(\/-(Key("in")), \/-(Key("i2")), -\/("<function>"))), srcNames))
      }
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
          ConfigResult.QueryFailure(Map(Key("omg.1") -> None), Set.empty, Vector(s.name)))
      }

      'caseInsensitive {
        val c = Config.get[String]("S2")
        assertEq((c tuple c.withCaseInsensitiveKeys).run(srcs).get_!, (None, Some("ah")))
      }
    }

    'consumerFn {
      class Mutable {
        var a = 1
        var b = 2
        var name = ""
        def setA(x: Int): Unit = a = x
        def setB(x: Int): Unit = b = x
        def setName(x: String): Unit = name = x
      }
      val c = Config.consumerFn[Mutable](
        _.get("in", _.setA),
        _.get("nope", _.setB),
        _.need("s", _.setName)
      ).map{ fn =>
        val m = new Mutable
        fn(m)
        m
      }
      val m = c.run(srcs).get_!
      assertEq((m.a, m.b, m.name), (100, 2, "hey"))
    }
    'consumerFn {
      class Mutable {
        var a = 1
        var b = 2
        var name = ""
      }
      val c = Config.consumerFn[Mutable](
        _.getC[Int]("in", _.a = _),
        _.getC[Int]("nope", _.b = _),
        _.getC[String]("s", _.name = _)
      ).map{ fn =>
        val m = new Mutable
        fn(m)
        m
      }
      val m = c.run(srcs).get_!
      assertEq((m.a, m.b, m.name), (100, 2, "hey"))
    }

  }
}
