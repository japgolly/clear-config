package japgolly.microlibs.utils

import utest._

object BiMapTest extends TestSuite {
  override def tests = TestSuite {
    "Adding & retrieving" - {
      val b = BiMap.newBuilder[String, Int]
      b += ("Three" -> 3)
      b("Two") = 2
      val m = b.result()
      assert(m.backward(3) == "Three")
      assert(m.forward("Two") == 2)
    }
  }
}
