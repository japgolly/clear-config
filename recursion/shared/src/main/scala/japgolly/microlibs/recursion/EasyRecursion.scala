package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse}

/**
  * Beginner-friendly. No Greek.
  */
object EasyRecursion {


  def fold[F[_]: Functor, A](alg: Algebra[F, A])(f: Fix[F]): A =
    Recursion.cata(alg)(f)

  def unfold[F[_]: Functor, A](coalg: Coalgebra[F, A])(a: A): Fix[F] =
    Recursion.ana(coalg)(a)

  def unfoldIntoFold[F[_]: Functor, A, B](coalg: Coalgebra[F, A], alg: Algebra[F, B])(a: A): B =
    Recursion.hylo(coalg, alg)(a)


  def monadicFold[M[_]: Monad, F[_]: Traverse, A](alg: AlgebraM[M, F, A])(f: Fix[F]): M[A] =
    Recursion.cataM(alg)(f)

  def monadicUnfold[M[_]: Monad, F[_]: Traverse, A](coalg: CoalgebraM[M, F, A])(a: A): M[Fix[F]] =
    Recursion.anaM(coalg)(a)

  def monadicUnfoldIntoFold[M[_]: Monad, F[_]: Traverse, A, B](coalg: CoalgebraM[M, F, A], alg: AlgebraM[M, F, B])(a: A): M[B] =
    Recursion.hyloM(coalg, alg)(a)

}
