package japgolly.microlibs

import scalaz.{Free, Cofree}

package object recursion {

  val Fix: FixModule = FixImpl
  type Fix[F[_]] = Fix.Fix[F]

  type FAlgebra[F[_], A] = F[A] => A
  type FCoalgebra[F[_], A] = A => F[A]

  type FAlgebraM[M[_], F[_], A] = F[A] => M[A]
  type FCoalgebraM[M[_], F[_], A] = A => M[F[A]]

  type RAlgebra[F[_], A] = F[(Fix[F], A)] => A
  type RCoalgebra[F[_], A] = A => F[Either[Fix[F], A]]

  /** Course-of-values algebra */
  type CVAlgebra[F[_], A] = F[Cofree[F, A]] => A

  /** Course-of-values co-algebra */
  type CVCoalgebra[F[_], A] = A => F[Free[F, A]]

  @inline implicit class FixOps[F[_]](private val self: Fix[F]) extends AnyVal {
    @inline def unfix: F[Fix[F]] =
      Fix.unfix(self)
  }

  @inline implicit def fAlgebraOps[F[_], A](self: F[A] => A): FAlgebraOps[F, A] =
    new FAlgebraOps(self)

  @inline implicit def fCoalgebraOps[F[_], A](self: A => F[A]): FCoalgebraOps[F, A] =
    new FCoalgebraOps(self)

}
