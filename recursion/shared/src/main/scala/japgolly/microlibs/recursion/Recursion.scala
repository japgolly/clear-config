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

  /** cata that transforms children before folding.
    * Top-most structure (i.e. the input) is not transformed.
    * Outside to inside.
    */
  def prepro[F[_], A](pre: F ~> F, alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.prepro(pre, alg).apply(f)

  /** ana that creates a structure, transforming each new child (i.e. the entire structure as exists at the end of a pass).
    * Top-most structure (i.e. the end result) is not transformed.
    * Inside to outside.
    */
  def postpro[F[_], A](coalg: Coalgebra[F, A], pro: F ~> F)(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.postpro(coalg, pro).apply(a)

  /** hylo that can short-circuit on construction */
  def elgot[F[_], A, B](elcoalg: A => B Either F[A], alg: Algebra[F, B])(a: A)(implicit F: Functor[F]): B =
    RecursionFn.elgot(elcoalg, alg).apply(a)

  /** hylo that can short-circuit on reduction */
  def coelgot[F[_], A, B](coalg: Coalgebra[F, A], elalg: (A, F[B]) => B)(a: A)(implicit F: Functor[F]): B =
    RecursionFn.coelgot(coalg, elalg).apply(a)

}
