package japgolly.microlibs.recursion

import utest._
import MathExpr.Helpers._

object RecursionTest extends TestSuite {

  val eg1: FM = add(2, add(3, 11))

  override def tests = TestSuite {

    'cata {
      val r = Recursion.cata(MathExpr.eval)(eg1)
      assert(r == 16)
    }

    'ana {
      val expr = Recursion.ana(MathExpr.plusOnes)(5)
      assert(expr == add(1, add(1, add(1, add(1, 1)))))
    }

    'hylo {
      val n = 8
      val m = Recursion.hylo(MathExpr.plusOnes, MathExpr.eval)(n)
      assert(n == m)
    }

    'prepro {
      'stopAboveFive {
        val l = 1 to 10 toList
        val t = FixList(l: _*)
        val a = Recursion.prepro[ListF[Int, ?], Int](FixList.stopAboveFive, FixList.sum)(t)
        val expect = l.takeWhile(_ <= 5).sum
        assert(a == expect)
      }

      'zeroOutOdds {
        val l = 1 to 10 toList
        val t = FixList(l: _*)
        val a = Recursion.prepro[ListF[Int, ?], Int](FixList.zeroOutOdds, FixList.sum)(t)
        // l.head because prepro doesn't transform it's input, only children
        val expect = l.head + l.tail.filter(_ % 2 == 0).sum
        assert(a == expect)
      }
    }

    'postpro {
      println("\npostpro")
      for (i <- 1 to 8) {
        val t = Recursion.postpro(MathExpr.add10, MathExpr.plusOnes)(i)
        println(s"$i: ${Recursion.cata(MathExpr.print)(t)}")
      }
      println()
//      val expr = Recursion.postpro(add10)(MathExpr.plusOnes)(5)
//      assert(expr == add(12, add(12, add(12, add(12, 12)))))
    }

  }
}
