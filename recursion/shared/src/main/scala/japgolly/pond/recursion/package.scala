package japgolly.pond

import scalaz.Functor

package object recursion {

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]

  @inline implicit def algebraOps[F[_], A](self: F[A] => A): AlgebraOps[F, A] =
    new AlgebraOps(self)

  @inline implicit def recursiveOps[T[_[_]]: Recursive, F[_]](self: T[F]): RecursiveOps[T, F] =
    new RecursiveOps(self)

//  @inline implicit def corecursiveOps[T[_[_]]: Corecursive, F[_]](self: F[T[F]]): CorecursiveOps[T, F] =
//    new CorecursiveOps(self)

  def fix[T[_[_]], F[_]](t: F[T[F]])(implicit F: Functor[F], T: Corecursive[T]): T[F] =
    T.fix(t)(F)

//  final class RecursiveOps[T[_[_]], F[_]](private val self: T[F]) extends AnyVal {
//  final class CorecursiveOps[T[_[_]], F[_]](private val self: F[T[F]]) extends AnyVal {

}
