package japgolly.microlibs.recursion

import japgolly.microlibs.recursion

sealed trait FixModule {
  type Fix[F[_]]

  def apply[F[_]](f: F[recursion.Fix[F]]): Fix[F]
  def unfix[F[_]](f: Fix[F]): F[recursion.Fix[F]]
}

private[recursion] object FixImpl extends FixModule {
  override type Fix[F[_]] = F[recursion.Fix[F]]

  override def apply[F[_]](f: F[recursion.Fix[F]]): Fix[F] = f
  override def unfix[F[_]](f: Fix[F]): F[recursion.Fix[F]] = f
}
