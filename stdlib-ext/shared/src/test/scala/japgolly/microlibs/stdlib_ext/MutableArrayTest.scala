package japgolly.microlibs.stdlib_ext

import utest._

object MutableArrayTest extends TestSuite {

  def ji(i: Int) = java.lang.Integer.valueOf(i)

  override def tests = TestSuite {

    'anyRef {
      def m = MutableArray(List(2, 1, 3).map(ji))
      'init   - assert(m.to[List].map(_.intValue) == List(2, 1, 3))
      'map    - assert(m.map(_.toString).to[List] == List("2", "1", "3"))
      'mapOut - assert(m.mapOut[String, List[String]](_.toString) == List("2", "1", "3"))
      'sort   - assert(m.sort.to[List].map(_.intValue) == List(1, 2, 3))
      'mapAV  - assert(m.map(_.intValue).to[List] == List(2, 1, 3))
    }

    'int {
      def m = MutableArray(List(2, 1, 3))
      'init   - assert(m.to[List] == List(2, 1, 3))
      'map    - assert(m.map(_.toString).to[List] == List("2", "1", "3"))
      'mapOut - assert(m.mapOut[String, List[String]](_.toString) == List("2", "1", "3"))
      'sort   - assert(m.sort.to[List] == List(1, 2, 3))
      'mapAR  - assert(m.map(ji).to[List].map(_.intValue) == List(2, 1, 3))
    }

  }
}
