package japgolly.microlibs.name_fn

trait NameImplicits {
  implicit def materializeNameFromString(body: String): Name =
    macro NameMacros.name

  implicit def materializeNameFnFromString(body: String): NameFn[Any] =
    macro NameMacros.nameFn

  implicit def nameFnFromString[A](a: A)(implicit ev: A => Name): NameFn[Any] =
    NameFn const ev(a)
}

final class NameMacros(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe.{Name => _, _}

  def pkg = q"_root_.japgolly.microlibs.name_fn"

  def name(body: c.Expr[String]): c.Expr[Name] =
    body match {
      case Expr(Literal(Constant(s: String))) =>
        c.Expr[Name](q"$pkg.Name.now($s)")
      case _ =>
        c.Expr[Name](q"$pkg.Name($body)")
    }

  def nameFn(body: c.Expr[String]): c.Expr[NameFn[Any]] =
    c.Expr[NameFn[Any]](q"$pkg.NameFn.const($body)")
}
