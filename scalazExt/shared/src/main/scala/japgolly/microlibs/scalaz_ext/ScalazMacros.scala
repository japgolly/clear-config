package japgolly.microlibs.scalaz_ext

import scalaz.Equal
import scala.reflect.macros.blackbox
import japgolly.microlibs.macroutils.MacroUtils

object ScalazMacros {
  def deriveEqual[A]: Equal[A] = macro ScalazMacros.quietDeriveEqual[A]
  def _deriveEqual[A]: Equal[A] = macro ScalazMacros.debugDeriveEqual[A]
}


class ScalazMacros(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  private val equal = c.typeOf[Equal[_]]

  def quietDeriveEqual[T: c.WeakTypeTag]: c.Expr[Equal[T]] = implDeriveEqual(false)
  def debugDeriveEqual[T: c.WeakTypeTag]: c.Expr[Equal[T]] = implDeriveEqual(true )
  def implDeriveEqual[T: c.WeakTypeTag](debug: Boolean): c.Expr[Equal[T]] = {
    if (debug) println()
    val T = weakTypeOf[T]
    val t = T.typeSymbol

    def caseClass0: Tree =
      q"_root_.scalaz.Equal.equal[$T]((_, _) => true)"

    def caseClass1up(params: List[Symbol]): Tree = {
      val init = Init()
      var cmps = Vector.empty[Tree]
      for (p <- params) {
        val (pn, pt) = nameAndType(T, p)
        val e = init.valImp(appliedType(equal, pt))
        cmps :+= q"$e.equal(a.$pn,b.$pn)"
      }
      val expr = cmps.reduce((a, b) => q"$a && $b")
      q"""
        ..$init
        _root_.scalaz.Equal.equal[$T]((a, b) => $expr)
      """
    }

    def adt: Tree = {
      val init = Init()
      val cases = crawlADT[CaseDef](T, p => {
        val pt = determineAdtType(T, p)
        tryInferImplicit(appliedType(equal, pt)).map { et =>
          val e = init.valDef(et)
          cq"x: $pt => b match {case y: $pt => $e.equal(x,y); case _ => false}"
        }
      }, p => {
        val pt = p.asType.toType
        val u = appliedType(equal, pt)
        fail(s"Implicit not found: $u")
      })
      init wrap q"_root_.scalaz.Equal.equal[$T]((a,b) => a match {case ..$cases})"
    }

    val impl =
      if (t.isClass && t.asClass.isCaseClass) {
        ensureConcrete(T)
        val params = primaryConstructorParams(T)
        if (params.isEmpty)
          caseClass0
        else
          caseClass1up(params)
      } else
        adt

    if (debug) println("\n" + showCode(impl) + "\n")
    c.Expr[Equal[T]](impl)
  }
}