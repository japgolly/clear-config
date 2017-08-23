package japgolly.microlibs.stdlib_ext

import utest._
import StdlibExt._

object ScalaExtTest extends TestSuite {
  override def tests = TestSuite {

    "indent(int)" - {
      assert("a".indent(2) == "  a")
      assert("a\nb".indent(2) == "  a\n  b")
      assert("a\n  b".indent(2) == "  a\n    b")
    }

    'vectorInsertBefore {
      for {
        vs <- List(Vector(), Vector(1), Vector(1, 2, 3))
        i  <- -2  to vs.length + 2
      } {
        val r = vs.insertBefore(i, 666)
        if (i >= 0 && i <= vs.length) {
          assert(r.isDefined)
          val n = r.get
          assert(n(i) == 666)
          assert(n.filterNot(_ == 666) == vs)
        } else
          assert(r.isEmpty)
      }
    }
  }
}
