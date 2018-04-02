package japgolly.microlibs.utils

import utest._

object ConciseIntSetFormatTest extends TestSuite {

  def test(expect: String)(i: Int, is: Int*): Unit = {
    val nes = is.toSet + i
    val actual = ConciseIntSetFormat(nes)
    assert(actual == expect)
  }

  override def tests = TestSuite {
    'single - test("3")               (3)
    'range  - test("5-8")             (5,6,7,8)
    'range2 - test("1-3,7-9")         (1,2,3,7,8,9)
    'jump   - test("2,4,9")           (2,9,4)
    'pair   - test("2,3")             (2,3)
    'combo1 - test("2,3,5-9,20")      (20,9,8,7,6,5,2,3)
    'combo2 - test("1-4,6,7,10,13-15")(1,2,3,4,6,7,10,13,14,15)
  }
}

