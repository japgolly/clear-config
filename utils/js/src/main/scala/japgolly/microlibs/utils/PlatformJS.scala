package japgolly.microlibs.utils

import japgolly.univeq.UnivEq
import scala.collection.mutable

object PlatformJS extends Platform {

  override def memo[A: UnivEq, B](f: A => B): A => B = {
    val cache = new mutable.HashMap[A, B]()
    a => cache.getOrElseUpdate(a, f(a))
  }

  override def looseMemo[A: UnivEq, B](): LooseMemo[A, B] = {
    val cache = new mutable.HashMap[A, B]()
    cache.getOrElseUpdate
  }

  override def memoInt[A](f: Int => A): Int => A =
    memo(f) // Could be better

  override def memoThunk[A](f: () => A): () => A = {
    lazy val a = f()
    () => a
  }
}
