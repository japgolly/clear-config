package japgolly.pond.nonemptiness

import japgolly.univeq.UnivEq
import scala.collection.{AbstractIterator, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Range
import scala.math.Ordering
import scalaz._
import scalaz.std.vector.{vectorEqual, vectorOrder}

final class NonEmptyVector[+A](val head: A, val tail: Vector[A]) {
  override def toString = "NonEmpty" + whole.toString

  override def hashCode = head.## * 31 + tail.##

  override def equals(o: Any) = o match {
    case that: NonEmptyVector[Any] => this.head == that.head && this.tail == that.tail
    case _ => false
  }

  def length: Int =
    tail.length + 1

  def unsafeApply(i: Int): A =
    if (i == 0)
      head
    else
      tail(i - 1)

  def apply(i: Int): Option[A] =
    try {
      Some(unsafeApply(i))
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  def init: Vector[A] =
    if (tail.isEmpty)
      Vector.empty
    else
      head +: tail.init

  def initNonEmpty: Option[NonEmptyVector[A]] = NonEmptyVector option init
  def tailNonEmpty: Option[NonEmptyVector[A]] = NonEmptyVector option tail

  def map[B](f: A => B): NonEmptyVector[B] =
    NonEmptyVector(f(head), tail map f)

  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] =
    reduceMapLeft1(f)(_ ++ _)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def foreachWithIndex[U](f: (A, Int) => U): Unit = {
    f(head, 0)
    var i = 0
    for (a <- tail) {
      i += 1
      f(a, i)
    }
  }

  def indices: Range =
    0 until length

  def forall(f: A => Boolean): Boolean =
    f(head) && tail.forall(f)

  def exists(f: A => Boolean): Boolean =
    f(head) || tail.exists(f)

  def find(f: A => Boolean): Option[A] =
    if (f(head)) Some(head) else tail.find(f)

  def mapTail[B >: A](f: Vector[A] => Vector[B]): NonEmptyVector[B] =
    NonEmptyVector(head, f(tail))

  def mapWithIndex[B](f: (A, Int) => B): NonEmptyVector[B] = {
    val h = f(head, 0)
    var i = 0
    var t = Vector.empty[B]
    for (a <- tail) {
      i += 1
      t :+= f(a, i)
    }
    NonEmptyVector(h, t)
  }

  def :+[B >: A](a: B): NonEmptyVector[B] =
    mapTail(_ :+ a)

  def +:[B >: A](a: B): NonEmptyVector[B] =
    NonEmptyVector(a, head +: tail)

  def ++[B >: A](as: GenTraversableOnce[B]): NonEmptyVector[B] =
    mapTail(_ ++ as)

  def ++[B >: A](b: NonEmptyVector[B]): NonEmptyVector[B] =
    ++(b.whole)

  def ++:[B >: A](as: Vector[B]): NonEmptyVector[B] =
    if (as.isEmpty) this else NonEmptyVector(as.head, as.tail ++ whole)

  def last: A =
    if (tail.isEmpty) head else tail.last

  def whole: Vector[A] =
    head +: tail

  def reverse: NonEmptyVector[A] =
    if (tail.isEmpty) this else NonEmptyVector.end(tail.reverse, head)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    tail.foldLeft(f(z, head))(f)

  def foldMapLeft1[B](g: A => B)(f: (B, A) => B): B =
    tail.foldLeft(g(head))(f)

  def reduceMapLeft1[B](f: A => B)(g: (B, B) => B): B =
    foldMapLeft1(f)((b, a) => g(b, f(a)))

  def reduce[B >: A](f: (B, B) => B): B =
    reduceMapLeft1[B](a => a)(f)

  // Reduce bullshit red in IntelliJ
//  def traverseD[L, B](f: A => L \/ B): L \/ NonEmptyVector[B] =
//    NonEmptyVector.traverse1.traverseU(this)(f)

  def intercalate[B >: A](b: B): NonEmptyVector[B] =
    intercalateF(b)(a => a)

  def intercalateF[B](b: B)(f: A => B): NonEmptyVector[B] = {
    val r = implicitly[CanBuildFrom[Nothing, B, Vector[B]]].apply()
    for (a <- tail) {
      r += b
      r += f(a)
    }
    NonEmptyVector(f(head), r.result())
  }

  def filter(f: A => Boolean): Option[NonEmptyVector[A]] =
    NonEmptyVector.option(whole filter f)

  def filterNot(f: A => Boolean): Option[NonEmptyVector[A]] =
    filter(!f(_))

  def iterator: Iterator[A] =
    whole.iterator

  def toStream = whole.toStream

  def mapToNES[B: UnivEq](f: A => B): NonEmptySet[B] =
    NonEmptySet force iterator.map(f).toSet

  def toNES[B >: A : UnivEq]: NonEmptySet[B] =
    NonEmptySet(head, tail.toSet[B])

  private def safeTrans[B](f: Vector[A] => Vector[B]): NonEmptyVector[B] =
    NonEmptyVector force f(whole)

  def sorted[B >: A](implicit ord: Ordering[B])       = safeTrans(_.sorted[B])
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]) = safeTrans(_ sortBy f)
  def sortWith(lt: (A, A) => Boolean)                 = safeTrans(_ sortWith lt)

  def partitionD[B, C](f: A => B \/ C): (NonEmptyVector[B], Vector[C]) \/ (Vector[B], NonEmptyVector[C]) = {
    var bs = Vector.empty[B]
    var cs = Vector.empty[C]
    for (a <- tail)
      f(a) match {
        case -\/(b) => bs :+= b
        case \/-(c) => cs :+= c
      }
    f(head) match {
      case -\/(b) => -\/((NonEmptyVector(b, bs), cs))
      case \/-(c) => \/-((bs, NonEmptyVector(c, cs)))
    }
  }

  def partitionB(f: A => Boolean): (NonEmptyVector[A], Vector[A]) = {
    var ts = Vector.empty[A]
    var fs = Vector.empty[A]
    for (a <- tail)
      if (f(a))
        ts :+= a
      else
        fs :+= a
    if (ts.nonEmpty)
      (NonEmptyVector force ts, fs)
    else
      (NonEmptyVector force fs, ts)
  }

  /**
   * Peels away elements from the end until there are no elements left.
   *
   * Example:
   *
   * NonEmptyVector(2,4,6,8) will yield
   *
   *   NonEmptyVector(2,4,6,8)
   *   NonEmptyVector(2,4,6)
   *   NonEmptyVector(2,4)
   *   NonEmptyVector(2)
   */
  def peelFromEnd: Iterator[NonEmptyVector[A]] =
    new AbstractIterator[NonEmptyVector[A]] {
      var cur: NonEmptyVector[A] = NonEmptyVector.this
      override def hasNext = cur ne null
      override def next() = {
        val r = cur
        cur = r.initNonEmpty.orNull
        r
      }
    }
}

// =====================================================================================================================

object NonEmptyVector extends NonEmptyVectorImplicits0 {
  def one[A](h: A): NonEmptyVector[A] =
    new NonEmptyVector(h, Vector.empty)

  /** Avoids failed type-inference with NonEmptyVector(Vector.empty[Int], Vector.empty[Int]) */
  def varargs[A](h: A, t: A*): NonEmptyVector[A] =
    apply(h, t.toVector)

  def apply[A](h: A, t: A*): NonEmptyVector[A] =
    apply(h, t.toVector)

  def apply[A](h: A, t: Vector[A]): NonEmptyVector[A] =
    new NonEmptyVector(h, t)

  def endOV[A](init: Option[Vector[A]], last: A): NonEmptyVector[A] =
    init.fold(one(last))(end(_, last))

  def endO[A](init: Option[NonEmptyVector[A]], last: A): NonEmptyVector[A] =
    init.fold(one(last))(_ :+ last)

  def end[A](init: Vector[A], last: A): NonEmptyVector[A] =
    if (init.isEmpty)
      one(last)
    else
      new NonEmptyVector(init.head, init.tail :+ last)

  def maybe[A, B](v: Vector[A], empty: => B)(f: NonEmptyVector[A] => B): B =
    if (v.isEmpty) empty else f(NonEmptyVector(v.head, v.tail))

  def option[A](v: Vector[A]): Option[NonEmptyVector[A]] =
    maybe[A, Option[NonEmptyVector[A]]](v, None)(Some.apply)

  def force[A](v: Vector[A]): NonEmptyVector[A] =
    apply(v.head, v.tail)

  def unwrapOption[A](o: Option[NonEmptyVector[A]]): Vector[A] =
    o.fold(Vector.empty[A])(_.whole)

  def newBuilder[A](head: A): Builder[A] =
    new Builder(head)

  def newBuilderNE[A](as: NonEmptyVector[A]): Builder[A] = {
    val b = newBuilder(as.head)
    b ++= as.tail
    b
  }

  final class Builder[A](head: A) {
    private[this] val tail = Vector.newBuilder[A]

    def +=(a: A): Unit = {
      tail += a
      ()
    }

    def ++=(as: TraversableOnce[A]): Unit = {
      tail ++= as
      ()
    }

    def ++=(as: NonEmptyVector[A]): Unit = {
      this += as.head
      this ++= as.tail
    }

    def result(): NonEmptyVector[A] =
      NonEmptyVector(head, tail.result())
  }

  implicit def univEq[A: UnivEq]: UnivEq[NonEmptyVector[A]] =
    UnivEq.force

  implicit def semigroup[A]: Semigroup[NonEmptyVector[A]] =
    new Semigroup[NonEmptyVector[A]] {
      override def append(a: NonEmptyVector[A], b: => NonEmptyVector[A]): NonEmptyVector[A] = a ++ b
    }

  implicit def traverse1: Traverse1[NonEmptyVector] = new Traverse1[NonEmptyVector] {
    override def foldLeft[A, B](fa: NonEmptyVector[A], z: B)(f: (B, A) => B): B =
      fa.foldLeft(z)(f)

    override def foldMapRight1[A, B](fa: NonEmptyVector[A])(z: A => B)(f: (A, => B) => B): B =
      fa.init.reverseIterator.foldLeft(z(fa.last))((b, a) => f(a, b))

    override def index[A](fa: NonEmptyVector[A], i: Int): Option[A] =
      fa(i)

    override def length[A](fa: NonEmptyVector[A]) =
      fa.length

    override def map[A, B](fa: NonEmptyVector[A])(f: A => B): NonEmptyVector[B] =
      fa map f

    override def traverse1Impl[G[_], A, B](fa: NonEmptyVector[A])(f: A => G[B])(implicit ap: Apply[G]): G[NonEmptyVector[B]] = {
      val gh = f(fa.head)
      if (fa.tail.isEmpty)
        ap.map(gh)(one)
      else {
        val gz = ap.map(gh)(_ => Vector.empty[B])
        val gt = fa.tail.foldLeft(gz)((q, a) => ap.apply2(q, f(a))(_ :+ _))
        ap.apply2(gh, gt)(new NonEmptyVector(_, _))
      }
    }
  }

  object Sole {
    def unapply[A](v: NonEmptyVector[A]) = new Unapply(v)
    final class Unapply[A](val v: NonEmptyVector[A]) extends AnyVal {
      def isEmpty = v.tail.nonEmpty
      def get     = v.head
    }
  }
}

trait NonEmptyVectorImplicits1 {
  implicit def order[A: Order]: Order[NonEmptyVector[A]] =
    vectorOrder[A].contramap(_.whole)
}

trait NonEmptyVectorImplicits0 extends NonEmptyVectorImplicits1 {
  implicit def equality[A: Equal]: Equal[NonEmptyVector[A]] =
    vectorEqual[A].contramap(_.whole)
}
