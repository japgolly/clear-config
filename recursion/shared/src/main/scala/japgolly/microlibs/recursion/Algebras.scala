package japgolly.microlibs.recursion

import scalaz.{-\/, Free, Functor, Monad, \/, \/-}

final class FAlgebraOps[F[_], A](private val self: FAlgebra[F, A]) extends AnyVal {

  def toFAlgebraM[M[_]](implicit M: Monad[M]): FAlgebraM[M, F, A] =
    fa => M.point(self(fa))

  def toRAlgebra(implicit F: Functor[F]): RAlgebra[F, A] =
    ffa => self(F.map(ffa)(_._2))

  def toCVAlgebra(implicit F: Functor[F]): CVAlgebra[F, A] =
    fa => self(F.map(fa)(_.head))

  def zip[B](that: FAlgebra[F, B])(implicit F: Functor[F]): FAlgebra[F, (A, B)] =
    fab => {
      val a = self(F.map(fab)(_._1))
      val b = that(F.map(fab)(_._2))
      (a, b)
    }
}

final class FCoalgebraOps[F[_], A](private val self: FCoalgebra[F, A]) extends AnyVal {

  def toFCoalgebraM[M[_]](implicit M: Monad[M]): FCoalgebraM[M, F, A] =
    a => M.point(self(a))

  def toRCoalgebra(implicit F: Functor[F]): RCoalgebra[F, A] =
    a => F.map(self(a))(Right(_))

  def toCVCoalgebra(implicit F: Functor[F]): CVCoalgebra[F, A] =
    a => F.map(self(a))(Free.pure)

  def cozip[B](that: FCoalgebra[F, B])(implicit F: Functor[F]): FCoalgebra[F, A \/ B] = {
    case -\/(a) => F.map(self(a))(-\/(_))
    case \/-(b) => F.map(that(b))(\/-(_))
  }
}