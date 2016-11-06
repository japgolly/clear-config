package japgolly.pond.recursion

import scalaz.Functor

// extends AnyVal breaks compilation
final case class Fix[F[_]](unfix: F[Fix[F]])
object Fix {
  implicit val Recursive: Recursive[Fix] =
    new Recursive[Fix] {
      override def unfix[F[_]](t: Fix[F])(implicit F: Functor[F]) = t.unfix
    }

  implicit val Corecursive: Corecursive[Fix] =
    new Corecursive[Fix] {
      override def fix[F[_]](f: F[Fix[F]])(implicit F: Functor[F]) = Fix[F](f)
    }
}

trait Mu[F[_]] {
  def apply[A](algebra: Algebra[F, A]): A
}
object Mu {

//  lambek :: (Recursive t, Corecursive t) => (t -> Base t t)
//  lambek = cata (fmap embed)

  def lambek[T[_[_]], F[_]](t: T[F])(implicit r: Recursive[T], c: Corecursive[T], f: Functor[F]): F[T[F]] =
    r.cata[F, F[T[F]]](f.map(_)(c.fix(_)))(t)

  implicit val Recursive: Recursive[Mu] =
    new Recursive[Mu] {
      override def unfix[F[_]](t: Mu[F])(implicit F: Functor[F]): F[Mu[F]] =
        lambek(t)
      override def cata[F[_], A](alg: Algebra[F, A])(t: Mu[F])(implicit F: Functor[F]): A =
        t(alg)
    }

  implicit val Corecursive: Corecursive[Mu] =
    new Corecursive[Mu] {
      override def fix[F[_]](f: F[Mu[F]])(implicit F: Functor[F]): Mu[F] =
        new Mu[F] {
          override def apply[A](alg: Algebra[F, A]): A =
            alg(F.map(f)(_(alg)))
        }

    }
}

trait Nu[F[_]] {
  type A
  def apply(coalgebra: Coalgebra[F, A], a: A): Nu[F]
}

//trait Recursive[T[_[_]]] {
//  def unfix[F[_]](t: T[F]): F[T[F]]
//}
//trait Corecursive[T[_[_]]] {
//  def fix[F[_]](f: F[T[F]]): T[F]
//}

trait Recursive[T[_[_]]] {
  def unfix[F[_]](t: T[F])(implicit F: Functor[F]): F[T[F]]

  def cata[F[_], A](alg: Algebra[F, A])(t: T[F])(implicit F: Functor[F]): A =
    alg(F.map(unfix(t))(cata(alg)(_)(F)))
}
trait Corecursive[T[_[_]]] {
  def fix[F[_]](f: F[T[F]])(implicit F: Functor[F]): T[F]
}

final class RecursiveOps[T[_[_]], F[_]](private val self: T[F]) extends AnyVal {
  def unfix(implicit F: Functor[F], T: Recursive[T]): F[T[F]] =
    T.unfix(self)(F)
  def cataX[A](alg: Algebra[F, A])(implicit F: Functor[F], T: Recursive[T]): A =
    Recursion.cata(alg)(self)(F, T)
}
//final class CorecursiveOps[T[_[_]], F[_]](private val self: F[T[F]]) extends AnyVal {
//  def fix(implicit F: Functor[F], T: Corecursive[T]): T[F] =
//    T.fix(self)(F)
//}