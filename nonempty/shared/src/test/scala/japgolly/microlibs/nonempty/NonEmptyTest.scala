package japgolly.microlibs.nonempty

import utest._

object NonEmptyTest extends TestSuite {

  case class X()
  case class Y()

  override def tests = TestSuite {
    'vector - {
      'empty - assert(NonEmpty(Vector()).isEmpty)
      'one - assert(NonEmpty(Vector(1)) == Some(NonEmptyVector(1)))
      'two - assert(NonEmpty(Vector(1, 2)) == Some(NonEmptyVector(1, 2)))
    }
    'set - {
      'empty - assert(NonEmpty(Set.empty[Int]).isEmpty)
      'one - assert(NonEmpty(Set(1)) == Some(NonEmptySet(1)))
      'two - assert(NonEmpty(Set(1, 2)) == Some(NonEmptySet(1, 2)))
    }
    'map - {
      'empty - assert(NonEmpty(Map.empty[X, Y]).isEmpty)
      val ne = Map(X() -> Y())
      'nonEmpty - assert(NonEmpty(ne) == Some(NonEmpty.force(ne)))
    }
  }
}
