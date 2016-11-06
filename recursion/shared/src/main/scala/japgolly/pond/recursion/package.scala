package japgolly.pond

package object recursion {

  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]

//  @inline implicit def algebraOps[F[_], A](self: Algebra[F, A]): AlgebraOps[F, A] =
  @inline implicit def algebraOps[F[_], A](self: F[A] => A): AlgebraOps[F, A] =
    new AlgebraOps(self)

}
