package japgolly.microlibs

package object recursion {

  val Fix: FixModule = FixImpl
  type Fix[F[_]] = Fix.Fix[F]

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]

  @inline implicit class FixOps[F[_]](private val self: Fix[F]) extends AnyVal {
    @inline def unfix: F[Fix[F]] =
      Fix.unfix(self)
  }

  @inline implicit def algebraOps[F[_], A](self: F[A] => A): AlgebraOps[F, A] =
    new AlgebraOps(self)

  @inline implicit def coalgebraOps[F[_], A](self: A => F[A]): CoalgebraOps[F, A] =
    new CoalgebraOps(self)

}
