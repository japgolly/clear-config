package japgolly.microlibs.recursion

import scalaz.{Functor, ~>}

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

object FixList {
  type FixList[A] = Fix[ListF[A, ?]]

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
