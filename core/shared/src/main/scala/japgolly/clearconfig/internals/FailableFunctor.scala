package japgolly.clearconfig.internals

import scalaz.{-\/, \/, \/-}

trait FailableFunctor[F[_], A] {
  def mapAttempt[B](f: A => String \/ B): F[B]

  def map[B](f: A => B): F[B] =
    mapAttempt(a => \/-(f(a)))

  def test(errorMsg: A => Option[String]): F[A] =
    mapAttempt(a => errorMsg(a).fold[String \/ A](\/-(a))(-\/.apply))

  def ensure(test: A => Boolean, errorMsg: => String): F[A] =
    this.test(a => if (test(a)) None else Some(errorMsg))

  def mapCatch[B](f: A => B, e: Throwable => String = _.toString): F[B] =
    mapAttempt(a => \/.fromTryCatchNonFatal(f(a)).leftMap(e))

  def mapOption[B](f: A => Option[B], errorMsg: => String = "Not a recognised value."): F[B] =
    mapAttempt(f(_).fold[String \/ B](-\/(errorMsg))(\/-.apply))

  final def ensure_>=(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.gteq(_, rhs), s"Must be ≥ $rhs.")

  final def ensure_>(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.gt(_, rhs), s"Must be > $rhs.")

  final def ensure_<=(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.lteq(_, rhs), s"Must be ≤ $rhs.")

  final def ensure_<(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.lt(_, rhs), s"Must be < $rhs.")

}
