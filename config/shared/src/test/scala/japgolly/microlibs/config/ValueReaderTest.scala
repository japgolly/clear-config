package japgolly.microlibs.config

import utest._
import japgolly.microlibs.testutil.TestUtil._
import scalaz.{-\/, Equal, \/-}
import scalaz.std.AllInstances._

object ValueReaderTest extends TestSuite {

  def testOk[A: ValueReader: Equal](origValue: String, expect: A): Unit =
    assertEq(ValueReader[A].read(ConfigValue.Found(origValue)), \/-(expect))

  def testBad[A: ValueReader : Equal](origValue: String, errorFrag: String = ""): Unit =
    ValueReader[A].read(ConfigValue.Found(origValue)) match {
      case -\/(e) => assertContainsCI(e, errorFrag)
      case \/-(a) => fail(s"Error expected containing '$errorFrag', instead it passed with $a.")
    }

  override def tests = TestSuite {
    'defaults {
      import ValueReader.Implicits.Defaults._
      'string {
        testOk("qWe", "qWe")
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
      'whitespace {
        testOk(" a b c  ", "a b c")
        testOk("   124  ", 124)
      }
      'comments {
        testOk("x # y # z", "x")
        testOk("36 # hehe what?! 1", 36)
        testOk("136 #", 136)
      }
    }
  }
}
