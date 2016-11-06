package japgolly.pond.recursion

import utest._
import MathExpr.Helpers._

object Test extends TestSuite {

  val eg1: FM = add(2, add(3, 11))

  override def tests = TestSuite {

    'cata {
      val r = MathExpr.eval.cata(eg1)
      assert(r == 16)
    }

  }
}
