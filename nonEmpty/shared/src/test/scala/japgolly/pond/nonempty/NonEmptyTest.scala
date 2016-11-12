package japgolly.pond.nonempty

import utest._

object NonEmptyTest extends TestSuite {

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
  }
}
