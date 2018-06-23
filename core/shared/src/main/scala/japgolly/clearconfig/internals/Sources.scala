package japgolly.clearconfig.internals

import scalaz.{Functor, ~>}

final case class Sources[F[_]](highToLowPri: Vector[Source[F]]) extends AnyVal {
  override def toString: String =
    s"Sources(${highToLowPri.map(_.name.value) mkString " > "})"

  def >(lowerPri: Sources[F]): Sources[F] =
    Sources(highToLowPri ++ lowerPri.highToLowPri)

  def <(higherPri: Sources[F]): Sources[F] =
    higherPri > this

  def reverse: Sources[F] =
    Sources(highToLowPri.reverse)

  def trans[G[_]: Functor](f: F ~> G): Sources[G] =
    Sources(highToLowPri.map(_ trans f))
}

object Sources {
  def empty[F[_]]: Sources[F] =
    apply(Vector.empty)

  def highToLowPri[F[_]](ss: Sources[F]*): Sources[F] =
    apply(ss.iterator.flatMap(_.highToLowPri).toVector)

  def lowToHighPri[F[_]](ss: Sources[F]*): Sources[F] =
    highToLowPri(ss.reverse: _*)
}
