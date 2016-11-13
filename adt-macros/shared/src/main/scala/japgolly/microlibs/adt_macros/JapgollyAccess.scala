package japgolly.microlibs.adt_macros

trait JapgollyAccess {
  val c: scala.reflect.macros.blackbox.Context
  import c.universe._

  lazy val SelectNonemptyPkg    = Select(Select(Select(Ident(termNames.ROOTPKG), TermName("japgolly")), TermName("microlibs")), TermName("nonempty"))
  lazy val SelectNonEmptyVector = Select(SelectNonemptyPkg, TermName("NonEmptyVector"))
  lazy val SelectNonEmptySet    = Select(SelectNonemptyPkg, TermName("NonEmptySet"))

}
