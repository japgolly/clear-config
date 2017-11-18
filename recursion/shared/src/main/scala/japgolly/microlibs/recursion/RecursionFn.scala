package japgolly.microlibs.recursion

import scalaz.Functor

object RecursionFn {

  def cata[F[_], A](alg: Algebra[F, A])(implicit F: Functor[F]): Fix[F] => A = {
    var self: Fix[F] => A = null
    self = f => alg(F.map(f.unfix)(self))
    self
  }

}
