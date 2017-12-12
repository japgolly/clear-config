package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse}

/**
  * Beginner-friendly. No Greek.
  */
object EasyRecursion {


  def fold[F[_]: Functor, A](alg: FAlgebra[F, A])(f: Fix[F]): A =
    Recursion.cata(alg)(f)

  def unfold[F[_]: Functor, A](coalg: FCoalgebra[F, A])(a: A): Fix[F] =
    Recursion.ana(coalg)(a)

  def unfoldIntoFold[F[_]: Functor, A, B](coalg: FCoalgebra[F, A], alg: FAlgebra[F, B])(a: A): B =
    Recursion.hylo(coalg, alg)(a)


  def monadicFold[M[_]: Monad, F[_]: Traverse, A](alg: FAlgebraM[M, F, A])(f: Fix[F]): M[A] =
    Recursion.cataM(alg)(f)

  def monadicUnfold[M[_]: Monad, F[_]: Traverse, A](coalg: FCoalgebraM[M, F, A])(a: A): M[Fix[F]] =
    Recursion.anaM(coalg)(a)

  def monadicUnfoldIntoFold[M[_]: Monad, F[_]: Traverse, A, B](coalg: FCoalgebraM[M, F, A], alg: FAlgebraM[M, F, B])(a: A): M[B] =
    Recursion.hyloM(coalg, alg)(a)

}
