package japgolly.pond.recursion

import scalaz.{Functor, Monad, Traverse, ~>}

object Recursion {

  def cata[F[_], A](alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A = {
    var self: Fix[F] => A = null
    self = f => alg(F.map(f.unfix)(self))
    self(f)
  }

  def cataM[F[_], M[_], A](alg: AlgebraM[M, F, A])(f: Fix[F])(implicit M: Monad[M], F: Traverse[F]): M[A] = {
    var self: Fix[F] => M[A] = null
    self = f => M.bind(F.traverse(f.unfix)(self))(alg)
    self(f)
  }

  def ana[F[_], A](coalg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] = {
    var self: A => Fix[F] = null
    self = a => Fix[F](F.map(coalg(a))(self))
    self(a)
  }

  def anaM[M[_], F[_], A](coalg: CoalgebraM[M, F, A])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[Fix[F]] = {
    var self: A => M[Fix[F]] = null
    self = a => M.bind(coalg(a))(fa => M.map(F.traverse(fa)(self))(Fix.apply[F]))
    self(a)
  }

  def hylo[F[_], A, B](coalg: Coalgebra[F, A], alg: Algebra[F, B])(a: A)(implicit F: Functor[F]): B = {
    var self: A => B = null
    self = a => alg(F.map(coalg(a))(self))
    self(a)
  }

  def hyloM[M[_], F[_], A, B](coalg: CoalgebraM[M, F, A], alg: AlgebraM[M, F, B])(a: A)(implicit M: Monad[M], F: Traverse[F]): M[B] = {
    var self: A => M[B] = null
    self = a => M.bind(coalg(a))(fa => M.bind(F.traverse(fa)(self))(alg))
    self(a)
  }

  def prepro[F[_], A](pro: F ~> F)(alg: Algebra[F, A])(f: Fix[F])(implicit F: Functor[F]): A = {
    var self: Fix[F] => A = null
    val algF: Algebra[F, Fix[F]] = f => Fix[F](pro(f))
    val inner: Fix[F] => A = f => self(cata(algF)(f))
    self = f => alg(F.map(f.unfix)(inner))
    self(f)
  }

  def postpro[F[_], A](pro: F ~> F)(coalg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] = {
    var self: A => Fix[F] = null
    val algF: Coalgebra[F, Fix[F]] = f => pro(f.unfix)
    val inner: A => Fix[F] = a => ana(algF)(self(a))
    self = a => Fix[F](F.map(coalg(a))(inner))
    self(a)
  }
}
