package japgolly.pond.recursion

import scalaz.{Functor, Monad}

final class AlgebraOps[F[_], A](private val self: Algebra[F, A]) extends AnyVal {

  def cata(f: Fix[F])(implicit F: Functor[F]): A =
    Recursion.cata(self)(f)(F)

  def toAlgebraM[M[_]](implicit M: Monad[M]): AlgebraM[M, F, A] =
    fa => M.point(self(fa))

  def zip[B](algB: Algebra[F, B])(implicit F: Functor[F]): Algebra[F, (A, B)] =
    fab => {
      val a = self(F.map(fab)(_._1))
      val b = algB(F.map(fab)(_._2))
      (a, b)
    }

}