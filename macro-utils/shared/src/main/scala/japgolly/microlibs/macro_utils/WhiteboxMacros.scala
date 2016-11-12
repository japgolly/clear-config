package japgolly.microlibs.macro_utils

abstract class WhiteboxMacroUtils extends MacroUtils {
  val c: scala.reflect.macros.whitebox.Context
  import c.universe._

  def extractStaticAnnotationArgs: List[Tree] =
    c.macroApplication match  {
      case Apply(Select(Apply(_, args), _), _) => args
      case x => fail(s"Unable to determine annotation args.\n${showRaw(x)}")
    }

  def replaceEmptyBodyInAnnotatedObject(annottees: Seq[c.Expr[Any]])(newBody: List[Tree]): Tree =
    annottees.map(_.tree) match {
      case List(q"object $objName extends $parent { ..$body }") if body.isEmpty =>
        q"object $objName extends $parent { ..$newBody }"
      case _ => fail("You must annotate an object definition with an empty body.")
    }
}
