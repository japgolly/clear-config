package japgolly.pond.recursion

import scalaz.Functor

object Recursion {

  def cata[T[_[_]], F[_], A](alg: Algebra[F, A])(t: T[F])(implicit F: Functor[F], T: Recursive[T]): A = {
    var self: T[F] => A = null
    self = t => alg(F.map(T unfix t)(self))
    self(t)
  }

//  def cata[F[_], A](alg: Algebra[F, A])(fix: Fix[F])(implicit F: Functor[F]): A = {
//    var self: Fix[F] => A = null
//    self = fix => alg(F.map(fix.unfix)(self))
//    self(fix)
////    alg(F.map(fix.unfix)(cata(alg)(_)(F)))
//  }

//  def cata[F[_], A](alg: Algebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
//    var self: Fix[F] => A = null
//    self = fix => alg(F.map(fix.unfix)(self))
//    self
//  }

}
