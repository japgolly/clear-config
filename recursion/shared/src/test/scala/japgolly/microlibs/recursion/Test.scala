package japgolly.microlibs.recursion

import utest._
import MathExpr.Helpers._
import scalaz.{Functor, ~>}

object Test extends TestSuite {

  sealed trait ListF[+A, +F]
  case class ConsF[+A, +F](head: A, tail: F) extends ListF[A, F]
  case object NilF extends ListF[Nothing, Nothing]
  object ListF {
    implicit def functor[A]: Functor[ListF[A, ?]] = new Functor[ListF[A, ?]] {
      override def map[B, C](fa: ListF[A, B])(f: B => C): ListF[A, C] =
        fa match {
          case ConsF(h, t) => ConsF(h, f(t))
          case NilF => NilF
        }
    }
  }
  type FixList[A] = Fix[ListF[A, ?]]
  object FixList {
    def apply[A](as: A*): FixList[A] =
      Recursion.ana[ListF[A, ?], List[A]](coalg)(as.toList)
    def coalg[A]: Coalgebra[ListF[A, ?], List[A]] = {
      case Nil => NilF
      case h :: t => ConsF(h, t)
    }
    val zeroOutOdds = Lambda[ListF[Int, ?] ~> ListF[Int, ?]] {
      case ConsF(n, t) if n % 2 == 1 => ConsF(0, t)
      case e => e
    }
    val stopAboveFive = Lambda[ListF[Int, ?] ~> ListF[Int, ?]] {
      case ConsF(n, _) if n > 5 => NilF
      case e => e
    }
    val sum: Algebra[ListF[Int, ?], Int] = {
      case ConsF(h, t) => h + t
      case NilF        => 0
    }
  }

  val eg1: FM = add(2, add(3, 11))

  val add10 = Lambda[MathExpr[?] ~> MathExpr[?]] {
    case MathExpr.Num(n) => MathExpr.Num(n + 10)
    case e@ MathExpr.Add(_, _) => e
  }

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
        val t = Recursion.postpro(add10, MathExpr.plusOnes)(i)
        println(s"$i: ${Recursion.cata(MathExpr.print)(t)}")
      }
      println()
//      val expr = Recursion.postpro(add10)(MathExpr.plusOnes)(5)
//      assert(expr == add(12, add(12, add(12, add(12, 12)))))
    }

  }
}
