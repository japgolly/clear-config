package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse, ~>}

object Recursion {

  def cata[F[_], A](alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.cata(alg).apply(f)

  def cataM[M[_], F[_], A](alg: AlgebraM[M, F, A])(f: Fix[F])(implicit M: Monad[M], F: Traverse[F]): M[A] =
    RecursionFn.cataM(alg).apply(f)

  def ana[F[_], A](coalg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.ana(coalg).apply(a)

  def anaM[M[_], F[_], A](coalg: CoalgebraM[M, F, A])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[Fix[F]] =
    RecursionFn.anaM(coalg).apply(a)

  /** ana with immediate cata */
  def hylo[F[_], A, B](coalg: Coalgebra[F, A], alg: Algebra[F, B])(a: A)(implicit F: Functor[F]): B =
    RecursionFn.hylo(coalg, alg).apply(a)

  def hyloM[M[_], F[_], A, B](coalg: CoalgebraM[M, F, A], alg: AlgebraM[M, F, B])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[B] =
    RecursionFn.hyloM(coalg, alg).apply(a)

  /** cata that transforms children (outside to inside) before folding */
  def prepro[F[_], A](pro: F ~> F, alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.prepro(pro, alg).apply(f)

  def postpro[F[_], A](pro: F ~> F, coalg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.postpro(pro, coalg).apply(a)

  /** hylo that can short-circuit on construction */
  def elgot[F[_], A, B](alg: Algebra[F, B], elcoalg: A => B Either F[A])(a: A)(implicit F: Functor[F]): B =
    RecursionFn.elgot(alg, elcoalg).apply(a)

  /** hylo that can short-circuit on reduction */
  def coelgot[F[_], A, B](coalg: Coalgebra[F, A], elalg: (A, F[B]) => B)(a: A)(implicit F: Functor[F]): B =
    RecursionFn.coelgot(coalg, elalg).apply(a)

}
