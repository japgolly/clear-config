package japgolly.microlibs

package object recursion {

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]

  @inline implicit def algebraOps[F[_], A](self: F[A] => A): AlgebraOps[F, A] =
    new AlgebraOps(self)

  @inline implicit def coalgebraOps[F[_], A](self: A => F[A]): CoalgebraOps[F, A] =
    new CoalgebraOps(self)

}
