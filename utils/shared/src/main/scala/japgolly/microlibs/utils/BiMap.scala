package japgolly.microlibs.utils

import japgolly.univeq.UnivEq
import scala.collection.immutable.IntMap

/**
 * Bidirectional maps between values of two key types.
 *
 * @since 31/05/2013
 */
final class BiMap[A, B] private (val forward: Map[A, B], val backward: Map[B, A]) {
  assert(forward.size == backward.size, s"forward.size (${forward.size}) ≠ backward.size (${backward.size})")

  override def toString = s"BiMap($forward, …)"

  override def hashCode = forward.##

  override def equals(obj: Any): Boolean =
    obj match {
      case x: BiMap[A, B] => this.forward == x.forward
      case _              => false
    }

  def isEmpty = forward.isEmpty

  @inline def nonEmpty = !isEmpty

  def size = forward.size

  def toMap[C](forwards: Boolean)
              (implicit ev1: Map[A, B] =:= Map[C, C], ev2: Map[B, A] =:= Map[C, C]): Map[C, C] =
    if (forwards) forward else backward

  def toSet(implicit ev: B =:= A): Set[A] = {
    val b = Set.newBuilder[A]
    forward.foreach { t =>
      b += t._1
      b += t._2
    }
    b.result()
  }

  def flip: BiMap[B, A] =
    new BiMap(backward, forward)
}

object BiMap {
  implicit def univEq[A, B]: UnivEq[BiMap[A, B]] =
    UnivEq.force

  def force[A: UnivEq, B: UnivEq](forward: Map[A, B])(backward: Map[B, A]): BiMap[A, B] =
    new BiMap(forward, backward)

  def apply[A: UnivEq, B: UnivEq](forward: Map[A, B]): BiMap[A, B] =
    force(forward) {
      var m = Map.empty[B, A]
      forward.foreach(t => m = m.updated(t._2, t._1))
      m
    }

  def empty[A: UnivEq, B: UnivEq]: BiMap[A, B] =
    force[A, B](Map.empty)(Map.empty)

  def singleton[A: UnivEq, B: UnivEq](a: A, b: B): BiMap[A, B] =
    force[A, B](Map.empty.updated(a, b))(Map.empty.updated(b, a))

  def index[A: UnivEq](as: TraversableOnce[A]): BiMap[A, Int] = {
    var i = 0
    val b = newBuilderInt[A]
    as.foreach { a =>
      b.update(a, i)
      i += 1
    }
    b.result()
  }

  // ===================================================================================================================

  abstract class AbstractBuilder[A: UnivEq, @specialized(Int) B: UnivEq] {
    protected def updateAB(a: A, b: B): Unit
    protected def updateBA(b: B, a: A): Unit
    def result(): BiMap[A, B]

    final def update(a: A, b: B): Unit = {
      updateAB(a, b)
      updateBA(b, a)
    }

    @inline final def +=(ab: (A, B)): Unit =
      update(ab._1, ab._2)

    final def ++=(abs: TraversableOnce[(A, B)]): Unit =
      abs.foreach(t => update(t._1, t._2))
  }

  final class Builder[A: UnivEq, B: UnivEq] extends AbstractBuilder[A, B] {
    private[this] var ab = Map.empty[A, B]
    private[this] var ba = Map.empty[B, A]
    override protected def updateAB(a: A, b: B) = ab = ab.updated(a, b)
    override protected def updateBA(b: B, a: A) = ba = ba.updated(b, a)
    override def result() = force(ab)(ba)
  }

  final class BuilderInt[A: UnivEq] extends AbstractBuilder[A, Int] {
    private[this] var ab = Map.empty[A, Int]
    private[this] var ba = IntMap.empty[A]
    override protected def updateAB(a: A, i: Int) = ab = ab.updated(a, i)
    override protected def updateBA(i: Int, a: A) = ba = ba.updated(i, a)
    override def result() = force(ab)(ba)
  }

  @inline def newBuilder[A: UnivEq, B: UnivEq] = new Builder[A, B]
  @inline def newBuilderInt[A: UnivEq] = new BuilderInt[A]
}