package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse, ~>}

object Recursion {

  def cata[F[_], A](alg: FAlgebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.cata(alg).apply(f)

  def cataM[M[_], F[_], A](alg: FAlgebraM[M, F, A])(f: Fix[F])(implicit M: Monad[M], F: Traverse[F]): M[A] =
    RecursionFn.cataM(alg).apply(f)

  def ana[F[_], A](coalg: FCoalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.ana(coalg).apply(a)

  def anaM[M[_], F[_], A](coalg: FCoalgebraM[M, F, A])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[Fix[F]] =
    RecursionFn.anaM(coalg).apply(a)

  /** ana with immediate cata */
  def hylo[F[_], A, B](coalg: FCoalgebra[F, A], alg: FAlgebra[F, B])(a: A)(implicit F: Functor[F]): B =
    RecursionFn.hylo(coalg, alg).apply(a)

  def hyloM[M[_], F[_], A, B](coalg: FCoalgebraM[M, F, A], alg: FAlgebraM[M, F, B])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[B] =
    RecursionFn.hyloM(coalg, alg).apply(a)

  /** cata that transforms children before folding.
    * Top-most structure (i.e. the input) is not transformed.
    * Outside to inside.
    */
  def prepro[F[_], A](pre: F ~> F, alg: FAlgebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.prepro(pre, alg).apply(f)

  /** ana that creates a structure, transforming each new child (i.e. the entire structure as exists at the end of a pass).
    * Top-most structure (i.e. the end result) is not transformed.
    * Inside to outside.
    */
  def postpro[F[_], A](coalg: FCoalgebra[F, A], pro: F ~> F)(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.postpro(coalg, pro).apply(a)

  /** hylo that can short-circuit on construction */
  def elgot[F[_], A, B](elcoalg: A => B Either F[A], alg: FAlgebra[F, B])(a: A)(implicit F: Functor[F]): B =
    RecursionFn.elgot(elcoalg, alg).apply(a)

  /** hylo that can short-circuit on reduction */
  def coelgot[F[_], A, B](coalg: FCoalgebra[F, A], elalg: (A, () => F[B]) => B)(a: A)(implicit F: Functor[F]): B =
    RecursionFn.coelgot(coalg, elalg).apply(a)

  /** cata that has access to current subtree (Fix[F]) as well as that subtree's folded result (A) */
  def para[F[_], A](alg: RAlgebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.para(alg).apply(f)

  /** ana that can branch / short-circuit */
  def apo[F[_], A](coalg: RCoalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.apo(coalg).apply(a)

  /** cata that retains values of all previous (i.e. child) steps */
  def histo[F[_], A](alg: CVAlgebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A =
    RecursionFn.histo(alg).apply(f)

  /** ana that can build multiple levels in a single pass */
  def futu[F[_], A](coalg: CVCoalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] =
    RecursionFn.futu(coalg).apply(a)

  /** hylo of futu into histo */
  def chrono[F[_], A, B](coalg: CVCoalgebra[F, A], alg: CVAlgebra[F, B])(a: A)(implicit F: Functor[F]): B =
    RecursionFn.chrono(coalg, alg).apply(a)

}
