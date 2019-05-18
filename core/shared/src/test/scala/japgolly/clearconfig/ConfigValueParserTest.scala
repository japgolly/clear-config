package japgolly.clearconfig

import japgolly.microlibs.testutil.TestUtil._
import java.net.URL
import scalaz.std.AllInstances._
import scalaz.{-\/, Equal, \/-}
import utest._
import Helpers._

object ConfigValueParserTest extends TestSuite {

  val k = Key("k")
  val pp = implicitly[ConfigValuePreprocessor]

  def testOk[A: ConfigValueParser: Equal](origValue: String, expect: A): Unit =
    assertEq(ConfigValueParser[A].parse(pp.run(origValue)), \/-(expect))

  def testBad[A: ConfigValueParser : Equal](origValue: String, errorFrag: String = ""): Unit =
    ConfigValueParser[A].parse(pp.run(origValue)) match {
      case -\/(e) => assertContainsCI(e, errorFrag)
      case \/-(a) => fail(s"Error expected containing '$errorFrag', instead it passed with $a.")
    }

  override def tests = Tests {

    'defaults {

      'string {
        testOk("qWe", "qWe")
      }

      'double {
        testOk[Double]("123", 123)
        testOk[Double]("-3", -3.0)
        testOk[Double]("1.2", 1.2)
        testOk[Double]("0.2", 0.2)
        testOk[Double](".2", 0.2)
        testBad[Double]("X")
        testBad[Double]("123X")
        testBad[Double]("X123")
        testBad[Double]("")
      }

      'int {
        testOk("123", 123)
        testOk("-3", -3)
        testBad[Int]("X")
        testBad[Int]("123X")
        testBad[Int]("X123")
        testBad[Int]("3.4")
        testBad[Int]("")
      }

      'long {
        testOk("123", 123L)
        testOk("-3", -3L)
        testBad[Long]("X")
      }

      'boolean {
        testOk("1", true)
        testOk("true", true)
        testOk("TRUE", true)
        testOk("t", true)
        testOk("T", true)
        testOk("enabled", true)

        testOk("0", false)
        testOk("false", false)
        testOk("False", false)
        testOk("f", false)
        testOk("F", false)
        testOk("disabled", false)

        testBad[Boolean]("what")
        testBad[Boolean]("")
      }

      'oneOf {
        sealed trait X
        case object A extends X
        case object B extends X
        implicit val eq = Equal.equalA[X]
        implicit val v = ConfigValueParser.oneOf[X]("A" -> A, "B" -> B).preprocessValue(_.toUpperCase)
        testOk[X]("a", A)
        testOk[X]("A", A)
        testOk[X]("b", B)
        testOk[X]("B", B)
        assertEq(v.parse("c").swap.toOption, Some("Legal values are: A, B."))
      }

//      'url {
//        testOk("http://google.com", new URL("http://google.com"))
//        testBad[URL]("x")
//      }

      'whitespace {
        testOk(" a b c  ", "a b c")
        testOk("   124  ", 124)
      }

      'comments {
        testOk("x # y # z", "x")
        testOk(" # y # z", "")
        testOk("# y # z", "")
        testOk("36 # hehe what?! 1", 36)
        testOk("136 #", 136)
        testOk("blah!@#!@#", "blah!@#!@#")
        testOk("blah!@#!@# ", "blah!@#!@#")
        testOk("blah!@#!@# # a password", "blah!@#!@#")
      }
    }

    'ensure {
      'ok - assertEq(
        ConfigDef.need("in")(ConfigValueParser[Int].ensure(_ < 150, "Must be < 150.")).run(srcs),
        ConfigResult.Success(100))

      'ko - assertEq(
        ConfigDef.need("in")(ConfigValueParser[Int].ensure(_ > 150, "Must be > 150.")).run(srcs),
        ConfigResult.QueryFailure(Map(Key("in") -> Some((src1.name, Lookup.Error("Must be > 150.", Some("100"))))), Set.empty, srcNames))
    }

  }
}
