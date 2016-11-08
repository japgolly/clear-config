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
  val a: A
  val coalgebra: Coalgebra[F, A]
  def fa: F[A] = coalgebra(a)
}

object Nu {
  def apply[F[_], A](c: Coalgebra[F, A])(a: A): Nu[F] = {
    val aa = a
    type AA = A
    new Nu[F] {
      override type A = AA
      override val a = aa
      override val coalgebra = c
    }
  }

  def colambek[T[_[_]], F[_]](t: F[T[F]])(implicit r: Recursive[T], c: Corecursive[T], f: Functor[F]): T[F] =
    c.ana[F, F[T[F]]](f.map(_)(r.unfix(_)))(t)

  implicit val Recursive: Recursive[Nu] =
    new Recursive[Nu] {
      override def unfix[F[_]](t: Nu[F])(implicit F: Functor[F]): F[Nu[F]] =
        F.map(t.fa)(Nu(t.coalgebra))
    }

  implicit val Corecursive: Corecursive[Nu] =
    new Corecursive[Nu] {
      override def fix[F[_]](f: F[Nu[F]])(implicit F: Functor[F]): Nu[F] =
        colambek[Nu, F](f)
      override def ana[F[_], A](alg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Nu[F] =
        Nu(alg)(a)
    }
}

//trait Recursive[T[_[_]]] {
//  def unfix[F[_]](t: T[F]): F[T[F]]
//}
//trait Corecursive[T[_[_]]] {
//  def fix[F[_]](f: F[T[F]]): T[F]
//}

trait Recursive[T[_[_]]] {
  def unfix[F[_]](t: T[F])(implicit F: Functor[F]): F[T[F]]

  def cata[F[_], A](alg: Algebra[F, A])(t: T[F])(implicit F: Functor[F]): A = {
    var self: T[F] => A = null
    self = t => alg(F.map(unfix(t))(self))
    self(t)
    // alg(F.map(unfix(t))(cata(alg)(_)(F)))
  }
}
trait Corecursive[T[_[_]]] {
  def fix[F[_]](f: F[T[F]])(implicit F: Functor[F]): T[F]

  def ana[F[_], A](alg: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): T[F] = {
    var self: A => T[F] = null
    self = a => fix[F](F.map(alg(a))(self))
    self(a)
    //fix(F.map(alg(a))(ana(alg)(_)(F)))
  }
}

final class RecursiveOps[T[_[_]], F[_]](private val self: T[F]) extends AnyVal {
  def unfix(implicit F: Functor[F], T: Recursive[T]): F[T[F]] =
    T.unfix(self)(F)
//  def cata[A](alg: Algebra[F, A])(implicit F: Functor[F], T: Recursive[T]): A =
//    T.cata(alg)(self)(F)
//  def cataX[A](alg: Algebra[F, A])(implicit F: Functor[F], T: Recursive[T]): A =
//    Recursion.cata(alg)(self)(F, T)
}
//final class CorecursiveOps[T[_[_]], F[_]](private val self: F[T[F]]) extends AnyVal {
//  def fix(implicit F: Functor[F], T: Corecursive[T]): T[F] =
//    T.fix(self)(F)
//  def ana[F[_], A](alg: Coalgebra[F, A])(a: A)(implicit F: Functor[F], T: Corecursive[T]): T[F] =
//    T.ana(alg)(a)
//}
