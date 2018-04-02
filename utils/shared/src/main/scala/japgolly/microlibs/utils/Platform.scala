package japgolly.microlibs.utils

import japgolly.univeq.UnivEq

trait Platform {

  def memo[A: UnivEq, B](f: A => B): A => B

  final type LooseMemo[A, B] = (A, => B) => B

  def looseMemo[A: UnivEq, B](): LooseMemo[A, B]

  def memoInt[A](f: Int => A): Int => A

  def memoThunk[A](f: () => A): () => A
}
