package japgolly.microlibs.utils

import nyaya.gen.Gen
import nyaya.prop.Prop
import nyaya.test.PropTest._
import utest._

object UtilsTest extends TestSuite {

  override def tests = TestSuite {

    'quickStringExists {
      val x = "x"
      val p: Prop[Set[String]] =
        Prop.test("quickStringExists", ss => {
          val f = Utils.quickStringExists(ss)
          (ss + "" + "123").toList
            .flatMap(s => s.drop(1) :: (s + x) :: (x + s + x) :: s :: Nil)
            .forall(s => {
              // println(s"${ss.contains(s)} / ${f(s)} -- [$s]")
              ss.contains(s) == f(s)
            })
        })
      Gen.string(0 to 8).set.mustSatisfy(p)
    }

    'quickStringLookup {
      val x = "x"
      val p: Prop[Map[String, Int]] =
        Prop.test("quickStringLookup", m => {
          val f = Utils.quickStringLookup(m)
          (m.keySet + "" + "123").toList
            .flatMap(s => s.drop(1) :: (s + x) :: (x + s + x) :: s :: Nil)
            .forall(s => {
              m.get(s) == f(s)
            })
        })
      Gen.string(0 to 8).mapTo(Gen.int).mustSatisfy(p)
    }

  }
}
