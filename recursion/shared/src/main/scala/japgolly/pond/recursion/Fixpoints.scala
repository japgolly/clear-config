package japgolly.pond.recursion

// extends AnyVal breaks compilation
final case class Fix[F[_]](unfix: F[Fix[F]])
