package japgolly.pond.recursion

import scalaz.Functor

sealed abstract class MathExpr[+A]
object MathExpr {
  case class Num(value: Int) extends MathExpr[Nothing]
  case class Add[+A](a: A, b: A) extends MathExpr[A]

  val eval: Algebra[MathExpr, Int] = {
    case Num(i)    => i
    case Add(a, b) => a + b
  }

  implicit val functor: Functor[MathExpr] =
    new Functor[MathExpr] {
      override def map[A, B](fa: MathExpr[A])(f: A => B) = fa match {
        case n: Num    => n
        case Add(a, b) => Add(f(a), f(b))
      }
    }

  class HelpersT[T[_[_]]: Recursive: Corecursive] {
    type FM = T[MathExpr]
    type MF = MathExpr[T[MathExpr]]
    implicit def autoFix[A](a: A)(implicit f: A => MF): FM = fix(f(a))
    implicit def num(i: Int): MF = Num(i)
    def add(a: FM, b: FM): FM = Add(a, b)
  }
  val HelpersFix = new HelpersT[Fix]
  val HelpersMu = new HelpersT[Mu]
  val HelpersNu = new HelpersT[Nu]
}
