package japgolly.microlibs.stdlib_ext

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/**
  * Scala arrays don't support in-place modification.
  */
final class MutableArray[A](val underlying: Array[Any]) {
  override def toString = underlying.mkString("MutableArray[", ", ", "]")

  def length = underlying.length
  def isEmpty = underlying.isEmpty
  def nonEmpty = underlying.nonEmpty
  def iterator = array.iterator
  def toIterable = array.toIterable

  def array: Array[A] =
    underlying.asInstanceOf[Array[A]]

  def widen[B >: A]: MutableArray[B] =
    this.asInstanceOf[MutableArray[B]]

  def map[B](f: A => B): MutableArray[B] = {
    val a = array
    var i = length
    while (i > 0) {
      i -= 1
      underlying(i) = f(a(i))
    }
    this.asInstanceOf[MutableArray[B]]
  }

  def mapOut[B, That](f: A => B)(implicit cbf: CanBuildFrom[Nothing, B, That]): That = {
    val b = cbf()
    b.sizeHint(length)
    for (a <- array)
      b += f(a)
    b.result()
  }

  def sort(implicit o: Ordering[A]): MutableArray[A] = {
    scala.util.Sorting.quickSort(array)(o)
    this
  }

  def sortBy[B: Ordering](f: A => B): MutableArray[A] =
    sort(Ordering by f)

  def sortBySchwartzian[B: Ordering](f: A => B): MutableArray[A] =
    map(a => (f(a), a))
      .sort(Ordering.by((_: (B, A))._1))
      .map(_._2)

  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] =
    mapOut[A, Col[A]](identity)
}

// =====================================================================================================================

object MutableArray {

  def apply[A](as: TraversableOnce[A]): MutableArray[A] =
    new MutableArray(as.toArray[Any])

  def map[A, B](as: Iterable[A])(f: A => B): MutableArray[B] =
    apply(as.toIterator.map(f))
}
