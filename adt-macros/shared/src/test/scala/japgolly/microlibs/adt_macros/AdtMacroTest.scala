package japgolly.microlibs.adt_macros

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import utest._
import aa._
import AdtMacros._
import japgolly.univeq.UnivEq

object AdtMacroTest extends TestSuite {

  /*
  def adtIso[Adt, T](f: Adt => T): AdtIso[Adt, T] = macro AdtMacros.quietAdtIso[Adt, T]
  def valuesForAdt[T, V](f: T => V): NonEmptyVector[V] = macro AdtMacros.quietValuesForAdt[T, V]
  def valuesForAdtF[T, V](f: T => V): (NonEmptyVector[V], T => V) = macro AdtMacros.quietValuesForAdtF[T, V]
   */

  def assertUnorderedNEV[A](actual: NonEmptyVector[A], expect: NonEmptyVector[A]): Unit = {
    val norm: NonEmptyVector[A] => NonEmptyVector[A] = _.sortBy(_.toString)
    val a = norm(actual)
    val e = norm(expect)
    assert(a == e)
  }

  def assertUnorderedNEV[A](actual: NonEmptyVector[A])(e1: A, eN: A*): Unit =
    assertUnorderedNEV(actual, NonEmptyVector(e1, eN.toVector))

  def assertFail(e: CompileError) = e.msg

  override def tests = TestSuite {

    'adtValues {
//      's1i - assertUnorderedNEV(MonoS1.Values)(MonoS1.A)
//      's3i - assertUnorderedNEV(MonoS3.Values)(MonoS3.A, MonoS3.B, MonoS3.C)
      's1 - assertUnorderedNEV(adtValues[MonoS1])(MonoS1.A)
      's3 - assertUnorderedNEV(adtValues[MonoS3])(MonoS3.A, MonoS3.B, MonoS3.C)
      'd1 - assertFail(compileError("adtValues[MonoD1]"))
      'unsealed - assertFail(compileError("adtValues[Unsealed]"))
    }

//    'adtValuesManual {
//      's1i - assertOrderedNEV(MonoS1.ValuesM)(MonoS1.A)
//      's3i - assertOrderedNEV(MonoS3.ValuesM)(MonoS3.A, MonoS3.B, MonoS3.C)
//      's1 - assertOrderedNEV(adtValuesManual[MonoS1](MonoS1.A))(MonoS1.A)
//      's3 - assertOrderedNEV(adtValuesManual[MonoS3](MonoS3.A, MonoS3.B, MonoS3.C))(MonoS3.A, MonoS3.B, MonoS3.C)
//      'dup - assertFail(compileError("adtValuesManual[MonoS1](MonoS1.A, MonoS1.A)"))
////      'd1 - assertFail(compileError("adtValues[MonoD1]"))
////      'unsealed - assertFail(compileError("adtValues[Unsealed]"))
//    }

    'valuesForAdt {
      import MonoD._
      'ok {
        assertUnorderedNEV(valuesForAdt[MonoD, String] {
          case _: A => "A"
          case _: B => "B"
          case C    => "C"
          case _: D => "D"
        })("A", "B", "C", "D")
      }
      'missing {
        assertFail(compileError("valuesForAdt[MonoD, String] {case _: A => \"A\"}"))
      }
      'dup {
        assertFail(compileError(
          """
            valuesForAdt[MonoD, String] {
              case _: A => "A1"
              case _: A => "A2"
              case _: B => "B"
              case C    => "C"
              case _: D => "D"
            }
          """))
      }
      'extra {
        assertFail(compileError(
          """
            valuesForAdt[MonoD, String] {
              case _: A => "A"
              case _: Int => "I"
              case _: B => "B"
              case C    => "C"
              case _: D => "D"
            }
          """))
      }
    }

    'adtIso {
      val (mc, cm, ms, cs) = adtIso[MonoS3, Char] {
        case MonoS3.A => 'a'
        case MonoS3.B => 'b'
        case MonoS3.C => 'c'
      }
      assertUnorderedNEV(ms)(MonoS3.A, MonoS3.B, MonoS3.C)
      assertUnorderedNEV(cs)('a', 'b', 'c')
      for (m <- ms)
        assert(cm(mc(m)) == m)
    }

    'adtIsoSet {
      implicit def univEqS3: UnivEq[MonoS3] = UnivEq.derive[MonoS3]
      val (mc, cm, ms, cs) = adtIsoSet[MonoS3, Char] {
        case MonoS3.A => 'a'
        case MonoS3.B => 'b'
        case MonoS3.C => 'c'
      }
      assert(ms == NonEmptySet[MonoS3](MonoS3.A, MonoS3.B, MonoS3.C))
      assert(cs == NonEmptySet('a', 'b', 'c'))
      for (m <- ms)
        assert(cm(mc(m)) == m)
    }

  }
}
