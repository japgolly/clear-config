package japgolly.microlibs.name_fn

final case class NameFn[-A](fn: Option[A] => Name) extends AnyVal {
  @inline def apply(i: Option[A]) =
    fn(i)

  def map(f: Name => Name): NameFn[A] =
    NameFn(f compose fn)

  def cmap[B](f: B => A): NameFn[B] =
    NameFn(ob => apply(ob map f))

  def comap[B](f: B => Option[A]): NameFn[B] =
    NameFn(ob => apply(ob flatMap f))

  def mapContextFree(n: Name): NameFn[A] =
    NameFn {
      case s@Some(_) => fn(s)
      case None      => n
    }
}

object NameFn {
  def const(n: Name): NameFn[Any] =
    NameFn(_ => n)
}
