package japgolly.microlibs.utils

import japgolly.univeq.UnivEq

/**
 * Wraps a value such that reference equality holds.
 *
 * Allows values to be used as map-keys, in sets, etc with uniqueness being by reference.
 */
final class Ref[A <: AnyRef](val value: A) {
  override def hashCode = value.##
  override def equals(other: Any) =
    other match {
      case r: Ref[A] => value eq r.value
      case _         => false
    }
}

object Ref {
  def apply[A <: AnyRef](a: A): Ref[A] =
    new Ref(a)

  implicit def refEquality[A <: AnyRef]: UnivEq[Ref[A]] =
    UnivEq.force
}
