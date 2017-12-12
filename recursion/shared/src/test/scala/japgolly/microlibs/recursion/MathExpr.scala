package japgolly.microlibs.recursion

import scalaz.{Functor, ~>}

sealed abstract class MathExpr[+A]
object MathExpr {
  case class Num(value: Int) extends MathExpr[Nothing]
  case class Add[+A](a: A, b: A) extends MathExpr[A]

  val eval: FAlgebra[MathExpr, Int] = {
    case Num(i)    => i
    case Add(a, b) => a + b
  }

  val print: FAlgebra[MathExpr, String] = {
    case Num(i)    => i.toString
    case Add(a, b) => s"($a + $b)"
  }

  val plusOnes: FCoalgebra[MathExpr, Int] =
    i => if (i < 2) MathExpr.Num(i) else MathExpr.Add(1, i - 1)

  implicit val functor: Functor[MathExpr] =
    new Functor[MathExpr] {
      override def map[A, B](fa: MathExpr[A])(f: A => B) = fa match {
        case n: Num    => n
        case Add(a, b) => Add(f(a), f(b))
      }
    }

  val add10 = Lambda[MathExpr[?] ~> MathExpr[?]] {
    case MathExpr.Num(n) => MathExpr.Num(n + 10)
    case e@ MathExpr.Add(_, _) => e
  }

  object Helpers {
    type FM = Fix[MathExpr]
    type MF = MathExpr[Fix[MathExpr]]
    implicit def autoFix[A](a: A)(implicit f: A => MF): FM = Fix(f(a))
    implicit def num(i: Int): MF = Num(i)
    def add(a: FM, b: FM): FM = Add(a, b)
  }
}
