package japgolly.microlibs.recursion

import scalaz.{Functor, Monad, Traverse}

/** Useful for situations where composite items need to be wrapped when nested, but not at the top level.
  *
  * Allows "2 * (1 + 1)" instead of "(2 * (1 + 1))".
  */
sealed abstract class AtomOrComposite[A] {
  def atom: A
}

object AtomOrComposite {

  final case class Atom[A](atom: A) extends AtomOrComposite[A]

  final case class Composite[A](composite: A, toAtom: A => A) extends AtomOrComposite[A] {
    override def atom = toAtom(composite)
  }

  def cata[F[_] : Functor, A](alg: FAlgebra[F, AtomOrComposite[A]])(f: Fix[F]): A =
    Recursion.cata(alg)(f) match {
      case Atom(a) => a
      case Composite(a, _) => a
    }

  def cataM[M[_] : Monad, F[_] : Traverse, A](alg: FAlgebraM[M, F, AtomOrComposite[A]])(f: Fix[F]): M[A] =
    Monad[M].map(Recursion.cataM(alg)(f)) {
      case Atom(a) => a
      case Composite(a, _) => a
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object string {
    def atom(a: String): Atom[String] =
      Atom(a)

    def composite(before: String, content: String, after: String): Composite[String] =
      Composite(content, before + _ + after)

    def composite(content: String): Composite[String] =
      composite("(", content, ")")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object stringBuilder {
    def atom(a: StringBuilder => Unit): Atom[StringBuilder => Unit] =
      Atom(a)

    def composite(before: StringBuilder => Unit, content: StringBuilder => Unit, after: StringBuilder => Unit): Composite[StringBuilder => Unit] =
      Composite(content, c => sb => {
        before(sb)
        c(sb)
        after(sb)
      })

    private val parenL: StringBuilder => Unit = sb => {sb append '('; ()}
    private val parenR: StringBuilder => Unit = sb => {sb append ')'; ()}

    def composite(content: StringBuilder => Unit): Composite[StringBuilder => Unit] =
      composite(parenL, content, parenR)
  }
}
