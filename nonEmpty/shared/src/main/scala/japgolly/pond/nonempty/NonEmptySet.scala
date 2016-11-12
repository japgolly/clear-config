package japgolly.pond.nonempty

import japgolly.univeq.UnivEq
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scalaz.Semigroup

/**
 * @param tail Does NOT contain head.
 */
final class NonEmptySet[A] private[nonempty] (val head: A, val tail: Set[A]) {
  private[this] implicit def univEq: UnivEq[A] = UnivEq.force

  override def toString = "NonEmpty" + whole.toString

  override def hashCode = head.## * 31 + tail.##

  override def equals(o: Any) = o match {
    case that: NonEmptySet[_] => this.whole == that.whole
    case _ => false
  }

  def size: Int =
    tail.size + 1

  def whole: Set[A] =
    tail + head

  def contains(a: A): Boolean =
    (head == a) || (tail contains a)

  def lacks(a: A): Boolean =
    !contains(a)

  def map[B: UnivEq](f: A => B): NonEmptySet[B] =
    NonEmptySet(f(head), tail map f)

  def flatMap[B: UnivEq](f: A => NonEmptySet[B]): NonEmptySet[B] =
    reduceMapLeft1(f)(_ ++ _)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def forall(f: A => Boolean): Boolean =
    f(head) && tail.forall(f)

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def +(a: A): NonEmptySet[A] =
    if (contains(a))
      this
    else
      new NonEmptySet(head, tail + a)

  def ++(as: GenTraversableOnce[A]): NonEmptySet[A] =
    NonEmptySet(head, tail ++ as)

  def ++(as: NonEmptySet[A]): NonEmptySet[A] =
    ++(as.whole)

  def last: A =
    if (tail.isEmpty) head else tail.last

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
    tail.foldLeft(g(head))(f)

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  def toStream = whole.toStream
  def toVector = whole.toVector

  def toNEV: NonEmptyVector[A] =
    NonEmptyVector(head, tail.toVector)

  def mapV[B](f: A => B): NonEmptyVector[B] = {
    val b = implicitly[CanBuildFrom[Nothing, B, Vector[B]]].apply()
    tail.foreach(b += f(_))
    NonEmptyVector(f(head), b.result())
  }

  def iterator: Iterator[A] =
    whole.iterator
}

// =====================================================================================================================

object NonEmptySet {
  def one[A: UnivEq](h: A): NonEmptySet[A] =
    new NonEmptySet(h, Set.empty)

  def apply[A: UnivEq](h: A, t: A*): NonEmptySet[A] =
    apply(h, t.toSet)

  def apply[A: UnivEq](h: A, t: Set[A]): NonEmptySet[A] =
    new NonEmptySet(h, t - h)

  def maybe[A: UnivEq, B](s: Set[A], empty: => B)(f: NonEmptySet[A] => B): B =
    if (s.isEmpty)
      empty
    else
      f(force(s))

  def option[A: UnivEq](s: Set[A]): Option[NonEmptySet[A]] =
    maybe[A, Option[NonEmptySet[A]]](s, None)(Some.apply)

  def force[A: UnivEq](s: Set[A]): NonEmptySet[A] = {
    val h = s.head
    // Until Scala 2.12, s-h is faster than .tail
    // apply() also performs s-h so we don't bother here
    apply(h, s)
  }

  def unwrapOption[A](o: Option[NonEmptySet[A]]): Set[A] =
    o.fold(Set.empty[A])(_.whole)

  implicit def univEq[A: UnivEq]: UnivEq[NonEmptySet[A]] =
    UnivEq.force

  implicit def semigroup[A]: Semigroup[NonEmptySet[A]] =
    new Semigroup[NonEmptySet[A]] {
      override def append(a: NonEmptySet[A], b: => NonEmptySet[A]): NonEmptySet[A] = a ++ b
    }

  object Sole {
    def unapply[A](v: NonEmptySet[A]) = new Unapply(v)
    final class Unapply[A](val v: NonEmptySet[A]) extends AnyVal {
      def isEmpty = v.tail.nonEmpty
      def get     = v.head
    }
  }
}
