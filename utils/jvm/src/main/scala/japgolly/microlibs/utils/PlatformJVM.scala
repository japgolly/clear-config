package japgolly.microlibs.utils

import japgolly.univeq.UnivEq
import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => J8Fn}
import scala.collection.immutable.IntMap

object PlatformJVM extends Platform {

  override def memo[A: UnivEq, B](f: A => B): A => B = {
    val cache = new ConcurrentHashMap[A, B](32)
    val mf    = new J8Fn[A, B] { override def apply(a: A): B = f(a) }
    a => cache.computeIfAbsent(a, mf)
  }

  override def looseMemo[A: UnivEq, B](): LooseMemo[A, B] = {
    val cache = new ConcurrentHashMap[A, B](32)
    (a, b) => cache.computeIfAbsent(a, new J8Fn[A, B] { override def apply(a: A): B = b })
  }

  override def memoInt[A](f: Int => A): Int => A = {
    val lock = new AnyRef
    var m = IntMap.empty[A]
    i => {

      def cacheMiss() = {
        val a = f(i)
        m = m.updated(i, a)
        a
      }

      def withLock =
        lock.synchronized(m.getOrElse(i, cacheMiss()))

      m.getOrElse(i, withLock)
    }
  }

  override def memoThunk[A](f: () => A): () => A = {
    val lock = new AnyRef
    var oa: Option[A] = None
    () => {
      def cacheMiss() = {
        val a = f()
        oa = Some(a)
        a
      }

      def withLock =
        lock.synchronized(oa.getOrElse(cacheMiss()))

      oa.getOrElse(withLock)
    }
  }
}
