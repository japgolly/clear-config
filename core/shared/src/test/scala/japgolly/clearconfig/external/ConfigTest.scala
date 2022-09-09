package japgolly.clearconfig.external

import cats.Id
import cats.instances.all._
import cats.syntax.all._
import japgolly.clearconfig.Helpers._
import japgolly.microlibs.testutil.TestUtil._
import utest._

object ConfigTest extends TestSuite {

  override def tests = Tests {

    "findFirst" -
      assertEq(ConfigDef.need[String]("s").run(srcs), ConfigResult.Success("hey"))

    "findSecond" -
      assertEq(ConfigDef.need[Int]("i2").run(srcs), ConfigResult.Success(22))

    "notFound" -
      assertEq(ConfigDef.get[Int]("notfound").run(srcs), ConfigResult.Success(Option.empty[Int]))

    "missing1" -
      assertEq(ConfigDef.need[Int]("missing").run(srcs), ConfigResult.QueryFailure(Map(Key("missing") -> None), Set.empty, srcNames))

    "missing2" -
      assertEq(
        (ConfigDef.need[Int]("no1"), ConfigDef.need[Int]("no2")).tupled.run(srcs),
        ConfigResult.QueryFailure(Map(Key("no1") -> None, Key("no2") -> None), Set.empty, srcNames))

    "valueFail1" -
      assertEq(
        ConfigDef.need[Int]("s").run(srcs),
        ConfigResult.QueryFailure(Map(Key("s") -> Some((src1.name, Lookup.Error("Int expected.", Some("hey"))))), Set.empty, srcNames))

    "valueFail2" -
      assertEq(
        ConfigDef.need[Int]("s2").run(srcs),
        ConfigResult.QueryFailure(Map(Key("s2") -> Some((src2.name, Lookup.Error("Int expected.", Some("ah"))))), Set.empty, srcNames))

    "errorMsg" - {
      "notFound" - assertEq(ConfigDef.need[Int]("QQ").run(srcs).toEither, Left(
        """
          |1 error:
          |  - No value for key [QQ]
          |
          |2 sources (highest to lowest priority):
          |  - S1
          |  - S2
        """.stripMargin.trim))

      "notFound2" - {
        val c = (
          ConfigDef.need[Int]("QQ"),
          ConfigDef.get[Int]("X"),
          ConfigDef.need[Int]("i"),
          ConfigDef.need[Int]("M")
        ).tupled
        assertEq(c.run(srcs).toEither, Left(
          """
            |2 errors:
            |  - No value for key [M]
            |  - No value for key [QQ]
            |
            |2 sources (highest to lowest priority):
            |  - S1
            |  - S2
          """.stripMargin.trim))
      }

      "errors2" - {
        val c = (ConfigDef.need[Int]("s"), ConfigDef.get[Int]("X")).tupled
        assertEq(c.run(srcs > srcE).toEither, Left(
          """
            |2 errors:
            |  - Error reading key [X] from source [SE]: This source is fake!
            |  - Error reading key [s] from source [S1] with value [hey]: Int expected.
            |
            |3 sources (highest to lowest priority):
            |  - S1
            |  - S2
            |  - SE
          """.stripMargin.trim))
      }

      "unkeyedErrors" - {
        val c1 = ConfigDef.need[Int]("in").map(_ + 1000).ensure(_ > 1150, "Must be > 1150.")
        val c2 = 7.pure[ConfigDef].ensure(_ > 10, "Must be > 10.")
        val c3 = (ConfigDef.need[Int]("in"), ConfigDef.need[Int]("i2")).mapN(_ + _).ensure(_ > 150, "Must be > 150.")
        val c = (c1, c2, c3).tupled
        assertEq(c.run(srcs > srcE).toEither, Left(
          """
            |3 errors:
            |  - Error using <function>, key [i2], key [in]: Must be > 150.
            |  - Error using <function>, key [in]: Must be > 1150.
            |  - Error using runtime value [7]: Must be > 10.
            |
            |3 sources (highest to lowest priority):
            |  - S1
            |  - S2
            |  - SE
          """.stripMargin.trim))
      }
    }

    "ensure" - {
      "read1" - {
        val c = ConfigDef.need[Int]("in")
        "ok" - assertEq(
          c.ensure_<(150).run(srcs),
          ConfigResult.Success(100))
        "ko" - assertEq(
          c.ensure_>(150).run(srcs),
          ConfigResult.QueryFailure(Map(Key("in") -> Some((src1.name, Lookup.Error("Must be > 150.", Some("100"))))), Set.empty, srcNames))
      }

      "readMap1" - {
        val c = ConfigDef.need[Int]("in").map(_ + 1000)
        "ok" - assertEq(
          c.ensure_>(1050).run(srcs),
          ConfigResult.Success(1100))
        "ko" - assertEq(
          c.ensure_>(1150).run(srcs),
          ConfigResult.QueryFailure(Map.empty, Set("Must be > 1150." -> Set(Right(Key("in")), Left("<function>"))), srcNames))
      }

      "read2" - {
        val c = (ConfigDef.need[Int]("in"), ConfigDef.need[Int]("i2")).mapN(_ + _)
        "ok" - assertEq(
          c.ensure_<(150).run(srcs),
          ConfigResult.Success(122))
        "ko" - assertEq(
          c.ensure_>(150).run(srcs),
          ConfigResult.QueryFailure(Map.empty, Set("Must be > 150." -> Set(Right(Key("in")), Right(Key("i2")), Left("<function>"))), srcNames))
      }
    }

    "keyMod" - {
      "prefix" - {
        val s = ConfigSource.manual[Id]("S")(
          "a.b.1" -> "AB-1", "a.1" -> "A!", "b.1" -> "B!", "1" -> "I",
          "a.b.2" -> "AB-2", "a.2" -> "A@", "b.2" -> "B@", "2" -> "II")
        val one = ConfigDef.need[String]("1")
        val two = ConfigDef.need[String]("2")

        "1" - assertEq((one, two.withPrefix("b.")).tupled.run(s).get_!, ("I", "B@"))
        "2" - assertEq((one.withPrefix("b."), two).tupled.run(s).get_!, ("B!", "II"))
        "3" - assertEq((one, two).tupled.withPrefix("b.").run(s).get_!, ("B!", "B@"))
        "4" - assertEq((one, two.withPrefix("b.")).tupled.withPrefix("a.").run(s).get_!, ("A!", "AB-2"))

        "missing" - assertEq(
          one.withPrefix("omg.").run(s),
          ConfigResult.QueryFailure(Map(Key("omg.1") -> None), Set.empty, Vector(s.name)))
      }

//      'caseInsensitive {
//        val c = ConfigDef.get[String]("S2")
//        assertEq((c tuple c.withCaseInsensitiveKeys).run(srcs).get_!, (None, Some("ah")))
//      }
    }

    "consumerFn" - {
      class Mutable {
        var a = 1
        var b = 2
        var name = ""
        def setA(x: Int): Unit = a = x
        def setB(x: Int): Unit = b = x
        def setName(x: String): Unit = name = x
      }
      val c = ConfigDef.consumerFn[Mutable](
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

    "consumerFnC" - {
      class Mutable {
        var a = 1
        var b = 2
        var name = ""
      }
      val c = ConfigDef.consumerFn[Mutable](
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

    "mapKeyQueries" - {
      val s = ConfigSource.manual[Id]("S")(
        "both.1" -> "YAY", "both_1" -> "NOPE",
        "db_port" -> "1234")
        .mapKeyQueries(k => List(k, k.replace('.', '_')))

      "alternate" - assertEq(ConfigDef.need[Int]("db.port").run(s).get_!, 1234)
      "priority" - assertEq(ConfigDef.need[String]("both.1").run(s).get_!, "YAY")
    }

    "choice" - {
      val ci = ConfigDef.need[Int]("C").ensure(1.to(2).contains, "Choose 1 or 2")
      val c1 = ConfigDef.need[String]("C1")
      val c2 = ConfigDef.need[String]("C2")
      val cc = ci.choose(i => if (i == 1) c1 else c2)
      val v1 = "see one"
      val v2 = "sea too"
      val s1 = ConfigSource.manual[Id]("S1")("C" -> "1", "C1" -> v1)
      val s2 = ConfigSource.manual[Id]("S2")("C" -> "2", "C2" -> v2)

      "c1" - {
        assertEq(cc.run(s1).get_!, v1)
        assertEq(cc.run(s1 > s2).get_!, v1)
      }
      "c2" - {
        assertEq(cc.run(s2).get_!, v2)
        assertEq(cc.run(s2 > s1).get_!, v2)
      }
    }

    "whenAtLeastOneKeySpecified" - {
      val c1 = ConfigDef.need[Int]("c.1")
      val c2 = ConfigDef.need[String]("c.2")
      val c3 = ConfigDef.getOrUse[Int]("c.3", 666)
      val c4 = ConfigDef.get[Int]("c.4")

      val m = Map("c.1" -> "123", "c.2" -> "abc")
      val s0 = ConfigSource.empty[Id]("S")
      val s1 = ConfigSource.manual[Id]("S", m - "c.2")
      val s2 = ConfigSource.manual[Id]("S", m)
      val s3 = ConfigSource.manual[Id]("S", m.updated("c.3", "3"))
      val s4 = ConfigSource.manual[Id]("S", m.updated("c.4", "4"))

      "get" - {
        val co = c4.whenAtLeastOneKeySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(None))
        "all" - assertEq(co.run(s4), ConfigResult.Success(Some(Some(4))))
      }

      "getOrUse" - {
        val co = c3.whenAtLeastOneKeySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(None))
        "all" - assertEq(co.run(s3), ConfigResult.Success(Some(3)))
      }

      "need" - {
        val co = c1.whenAtLeastOneKeySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(None))
        "all" - assertEq(co.run(s2), ConfigResult.Success(Some(123)))
      }

      "multi" - {
        val co = (c1, c2, c3, c4).tupled.whenAtLeastOneKeySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(None))
        "all" - assertEq(co.run(s2), ConfigResult.Success(Some((123, "abc", 666, None))))
        "some" - assertEq(co.run(s1), ConfigResult.QueryFailure(Map(Key("c.2") -> None), Set.empty, Vector(s1.name)))
      }
    }

    "whenFullySpecified" - {
      val c1 = ConfigDef.need[Int]("c.1")
      val c2 = ConfigDef.need[String]("c.2")
      val c3 = ConfigDef.getOrUse[Int]("c.3", 666)
      val c4 = ConfigDef.get[Int]("c.4")

      val m = Map("c.1" -> "123", "c.2" -> "abc")
      val s0 = ConfigSource.empty[Id]("S")
      val s1 = ConfigSource.manual[Id]("S", m - "c.2")
      val s2 = ConfigSource.manual[Id]("S", m)
      val s3 = ConfigSource.manual[Id]("S", m.updated("c.3", "3"))
      val s4 = ConfigSource.manual[Id]("S", m.updated("c.4", "4"))

      "get" - {
        val co = c4.whenFullySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(Some(None)))
        "all" - assertEq(co.run(s4), ConfigResult.Success(Some(Some(4))))
      }

      "getOrUse" - {
        val co = c3.whenFullySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(Some(666)))
        "all" - assertEq(co.run(s3), ConfigResult.Success(Some(3)))
      }

      "need" - {
        val co = c1.whenFullySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(None))
        "all" - assertEq(co.run(s2), ConfigResult.Success(Some(123)))
      }

      "multi" - {
        val co = (c1, c2, c3, c4).tupled.whenFullySpecified
        "none" - assertEq(co.run(s0), ConfigResult.Success(None))
        "all" - assertEq(co.run(s2), ConfigResult.Success(Some((123, "abc", 666, None))))
        "some" - assertEq(co.run(s1), ConfigResult.Success(None))
      }
    }

    "inlineProps" - {
      "ok" - {
        val src1 = ConfigSource.manual[Id]("a")(
          "x.1" -> "hehe",
          "INLINE" ->
            s"""
               |# hehe
               | x.2      = 123
               | x.3    = good stuff # nice
               |""".stripMargin
        )
        val src = src1.expandInlineProperties("INLINE")

        val cfgDef = (
          ConfigDef.need[String]("x.1"),
            ConfigDef.need[String]("x.2"),
            ConfigDef.need[String]("x.3")
          ).tupled.withReport

        val (xs, report) = cfgDef.run(src).getOrDie()
        assert(xs == (("hehe", "123", "good stuff")))
        assert(!report.full.contains("INLINE"))
        report.full
      }

      "ko" - {
        val src1 = ConfigSource.manual[Id]("a")(
          "x.1" -> "hehe",
          "INLINE" ->
            s"""
               |# hehe
               | x.1      = 123
               |""".stripMargin
        )
        val src = src1.expandInlineProperties("INLINE")

        val cfgDef = ConfigDef.need[String]("x.1")

        val result = cfgDef.run(src).toEither
        assert(result == Left("Error preparing source [SourceName(a)]: The following keys are defined at both the top-level and in INLINE: x.1."))
      }
    }

    "environment" - {
      val s = ConfigSource.environment[Id]
      val (v, _) = ConfigDef.get[String]("PATH").withReport.run(s).getOrDie()
      // println(r.obfuscateKeys(_.value.toUpperCase.contains("PASS")).full)
      assert(v.isDefined)
    }

  }
}
