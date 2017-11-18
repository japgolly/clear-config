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
      'stopAboveFive {
        val i = -3
        val a = Recursion.postpro[ListF[Int, ?], Int](FixList.ascStream, FixList.stopAboveFive)(i)
        val expect = (-3 to 5).toList
        assert(a == FixList(expect: _*))
      }

      'zeroOutOdds {
        val i = 93
        val a = Recursion.postpro[ListF[Int, ?], Int](FixList.ascStream, FixList.zeroOutOdds)(i)
        val aa = FixList.toList(a)
        val expect = List(93, 94, 0, 96, 0, 98, 0, 100)
        assert(aa == expect)
      }
    }

    'coelgot {
      'shortCircuit {
        var coalgs = Vector.empty[Int]
        var algs = Vector.empty[Int]
        val str = Recursion.coelgot[MathExpr, Int, String](
          i => {coalgs :+= i; MathExpr.plusOnes(i)},
          (i, f) => {
            algs :+= i
            if (i == 87)
              "stop!"
            else
              f() match {
                case MathExpr.Num(s) => s.toString
                case MathExpr.Add(a, b) => s"$a+$b"
              }
          })(90)
        assert(str == "1+1+1+stop!", coalgs.length < 10, algs.length < 10)
      }
    }

  }
}
