package japgolly.microlibs.utils

import japgolly.univeq.UnivEq

object Memo {
  // Because of annoying Intellij IDEA
  private def platform: Platform = japgolly.microlibs.utils.Platform

  def apply[A: UnivEq, B](f: A => B): A => B =
    platform memo f

  def apply[A: UnivEq, B: UnivEq, Z](f: (A, B) => Z): (A, B) => Z = {
    val m = apply[(A, B), Z](f.tupled)
    (a, b) => m((a, b))
  }

  def bool[A](f: Boolean => A): Boolean => A = {
    val t = f(true)
    val z = f(false)
    b => if (b) t else z
  }

  def int[A](f: Int => A): Int => A =
    platform memoInt f

  def thunk[A](a: => A): () => A =
    platform.memoThunk(() => a)

  def curry2[A: UnivEq, B: UnivEq, Z](f: A => B => Z): A => B => Z =
    Memo[A, B => Z](a => Memo(f(a)))

  def curry3[A: UnivEq, B: UnivEq, C: UnivEq, Z](f: A => B => C => Z): A => B => C => Z =
    Memo[A, B => C => Z](a => curry2(f(a)))

  def by[I, K](memoKey: I => K) = new By(memoKey)
  final class By[I, K] private[Memo] (private val memoKey: I => K) extends AnyVal {
    def apply[O](value: I => O)(implicit ev: UnivEq[K]): I => O = {
      val m = platform.looseMemo[K, O]()
      i => m(memoKey(i), value(i))
    }
  }

  def byRef[A <: AnyRef, B](f: A => B): A => B =
    by[A, Ref[A]](Ref.apply)(f)
}