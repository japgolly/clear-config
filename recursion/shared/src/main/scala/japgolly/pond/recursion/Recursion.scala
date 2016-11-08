package japgolly.pond.recursion

import scalaz.Functor

object Recursion {

  def cata[T[_[_]], F[_], A](alg: Algebra[F, A])(t: T[F])(implicit F: Functor[F], T: Recursive[T]): A =
    T.cata(alg)(t)(F)

  def ana[T[_[_]], F[_], A](alg: Coalgebra[F, A])(a: A)(implicit F: Functor[F], T: Corecursive[T]): T[F] =
    T.ana(alg)(a)(F)

  def hylo[F[_], A, B](coalg: Coalgebra[F, A], alg: Algebra[F, B])(a: A)(implicit F: Functor[F]): B = {
    var self: A ⇒ B = null
    self = a ⇒ alg(F.map(coalg(a))(self))
    self(a)
  }
}
