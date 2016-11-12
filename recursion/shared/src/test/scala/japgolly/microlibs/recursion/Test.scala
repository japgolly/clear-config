package japgolly.microlibs.recursion

import utest._
import MathExpr.Helpers._
import scalaz.~>

object Test extends TestSuite {

  val eg1: FM = add(2, add(3, 11))
  val add10 = Lambda[MathExpr[?] ~> MathExpr[?]] {
    case MathExpr.Num(n) => MathExpr.Num(n + 10)
    case e@ MathExpr.Add(_, _) => e
  }

  override def tests = TestSuite {

    'cata {
      val r = MathExpr.eval.cata(eg1)
      assert(r == 16)
    }

    'ana {
      val expr = MathExpr.plusOnes.ana(5)
      assert(expr == add(1, add(1, add(1, add(1, 1)))))
    }

    'hylo {
      val n = 8
      val m = Recursion.hylo(MathExpr.plusOnes, MathExpr.eval)(n)
      assert(n == m)
    }

    'prepro {
      println("\nprepro")
      for (i <- 1 to 8) {
        val t = MathExpr.plusOnes.ana(i)
        val a = Recursion.prepro(add10)(MathExpr.eval)(t)
        println(s"$i: $a ← ${MathExpr.print cata t}")
      }
      println()
//      val r = Recursion.prepro(add10)(MathExpr.eval)(eg1)
//      assert(r == 16 + 11 * (1 + 2 * 2))
    }

    'postpro {
      println("\npostpro")
      for (i <- 1 to 8) {
        val t = Recursion.postpro(add10)(MathExpr.plusOnes)(i)
        println(s"$i: ${MathExpr.print cata t}")
      }
      println()
//      val expr = Recursion.postpro(add10)(MathExpr.plusOnes)(5)
//      assert(expr == add(12, add(12, add(12, add(12, 12)))))
    }

  }
}
