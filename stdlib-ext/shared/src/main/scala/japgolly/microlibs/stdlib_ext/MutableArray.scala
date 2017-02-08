package japgolly.microlibs.stdlib_ext

import scala.collection.generic.CanBuildFrom

/**
  * Scala arrays don't support in-place modification.
  */
final class MutableArray[A](underlying: Array[Any]) {
  override def toString = underlying.mkString("MutableArray[", ", ", "]")

  def length = underlying.length
  def isEmpty = underlying.isEmpty
  def nonEmpty = underlying.nonEmpty

  private[this] var pendingMap: Option[Any => Any] = None

  def array: Array[A] = {
    pendingMap.foreach { f =>
      pendingMap = None
      var i = length
      while (i > 0) {
        i -= 1
        underlying(i) = f(underlying(i))
      }
    }
    underlying.asInstanceOf[Array[A]]
  }

  def widen[B >: A]: MutableArray[B] =
    this.asInstanceOf[MutableArray[B]]

  def iterator: Iterator[A] =
    pendingMap match {
      case None    => array.iterator
      case Some(f) => underlying.iterator.map(f(_).asInstanceOf[A])
    }

  def map[B](f: A => B): MutableArray[B] = {
    val g = f.asInstanceOf[Any => Any]
    pendingMap = Some(pendingMap.fold(g)(g.compose))
    this.asInstanceOf[MutableArray[B]]
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

  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = {
    val b = cbf()
    b.sizeHint(length)
    iterator.foreach(b += _)
    b.result()
  }
}

// =====================================================================================================================

object MutableArray {

  def apply[A](as: TraversableOnce[A]): MutableArray[A] =
    new MutableArray(as.toArray[Any])

  def map[A, B](as: Iterable[A])(f: A => B): MutableArray[B] =
    apply(as.toIterator.map(f))
}
