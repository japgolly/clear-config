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
    Recursion.ana[ListF[A, ?], List[A]](listCoalg)(as.toList)

  def toList[A](f: FixList[A]): List[A]  =
    Recursion.cata[ListF[A, ?], List[A]](listAlg)(f)

  def listAlg[A]: FAlgebra[ListF[A, ?], List[A]] = {
    case NilF => Nil
    case ConsF(h, t) => h :: t
  }

  def listCoalg[A]: FCoalgebra[ListF[A, ?], List[A]] = {
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

  val sum: FAlgebra[ListF[Int, ?], Int] = {
    case ConsF(h, t) => h + t
    case NilF        => 0
  }

  val ascStream: FCoalgebra[ListF[Int, ?], Int] =
    i => if (i > 100) NilF else ConsF(i, i + 1)
}
