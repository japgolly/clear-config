package japgolly.clearconfig.internals

import cats.implicits._

trait FailableFunctor[F[_], A] {
  def mapAttempt[B](f: A => Either[String, B]): F[B]

  def map[B](f: A => B): F[B] =
    mapAttempt(a => Right(f(a)))

  def test(errorMsg: A => Option[String]): F[A] =
    mapAttempt(a => errorMsg(a).fold[Either[String, A]](Right(a))(Left(_)))

  def ensure(test: A => Boolean, errorMsg: => String): F[A] =
    this.test(a => if (test(a)) None else Some(errorMsg))

  def mapCatch[B](f: A => B, e: Throwable => String = _.toString): F[B] =
    mapAttempt(a => Either.catchNonFatal(f(a)).leftMap(e))

  def mapOption[B](f: A => Option[B], errorMsg: => String = "Not a recognised value."): F[B] =
    mapAttempt(f(_).toRight(errorMsg))

  final def ensure_>=(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.gteq(_, rhs), s"Must be ≥ $rhs.")

  final def ensure_>(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.gt(_, rhs), s"Must be > $rhs.")

  final def ensure_<=(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.lteq(_, rhs), s"Must be ≤ $rhs.")

  final def ensure_<(rhs: A)(implicit o: Ordering[A]): F[A] =
    ensure(o.lt(_, rhs), s"Must be < $rhs.")

}
