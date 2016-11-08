package japgolly.pond.recursion

final case class Fix[F[_]](unfix: F[Fix[F]])

object Fix {
}