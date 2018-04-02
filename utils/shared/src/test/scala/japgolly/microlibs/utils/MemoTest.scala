package japgolly.microlibs.utils

import utest._

object MemoTest extends TestSuite {

  override def tests = TestSuite {
    var i = 0
    def inc() = {i += 1; i}

    def test(actual: Int, expect: Int): Unit =
      assert(actual == expect)

    'fn1 {
      val f = Memo((_: Int) + inc())
      test(f(0), 1)
      test(f(0), 1)
      test(f(8), 10)
      test(f(8), 10)
      test(f(5), 8)
      test(f(0), 1)
      test(f(8), 10)
      test(f(5), 8)
    }

    'int {
      val f = Memo.int(_ + inc())
      test(f(0), 1)
      test(f(0), 1)
      test(f(8), 10)
      test(f(8), 10)
      test(f(5), 8)
      test(f(0), 1)
      test(f(8), 10)
      test(f(5), 8)
    }

    'curry {
      val f = Memo.curry2((m: Int) => {
        val x = inc()
        (n: Int) => 1000*m +100*n + x*10 + inc()
      })
      test(f(7)(4), 7412)
      test(f(7)(4), 7412)
      test(f(7)(6), 7613)
      test(f(7)(6), 7613)
      test(f(4)(7), 4745)
      test(f(4)(6), 4646)
      test(f(4)(6), 4646)
      test(f(7)(4), 7412)
    }
  }
}
