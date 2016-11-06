package japgolly.pond.recursion

import utest._

object Test extends TestSuite {

  override def tests = TestSuite {

    'cata {
      import MathExpr.HelpersFix._
      val eg1: FM = add(2, add(3, 11))

      val r = MathExpr.eval.cata(eg1)
      assert(r == 16)
    }

    'mu {
      import MathExpr.HelpersMu._
      val eg1: FM = add(2, add(3, 11))
      val r = MathExpr.eval.cata(eg1)
      assert(r == 16)
    }
  }
}
