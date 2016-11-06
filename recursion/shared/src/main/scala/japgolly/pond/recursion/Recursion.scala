package japgolly.pond.recursion

import scalaz.Functor

object Recursion {

  def cata[F[_], A](alg: Algebra[F, A])(fix: Fix[F])(implicit F: Functor[F]): A = {
    var self: Fix[F] => A = null
    self = fix => alg(F.map(fix.unfix)(self))
    self(fix)
//    alg(F.map(fix.unfix)(cata(alg)(_)(F)))
  }

//  def cata[F[_], A](alg: Algebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
//    var self: Fix[F] => A = null
//    self = fix => alg(F.map(fix.unfix)(self))
//    self
//  }

}
