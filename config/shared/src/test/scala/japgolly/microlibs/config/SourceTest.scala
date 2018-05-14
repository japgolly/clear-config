package japgolly.microlibs.config

import scalaz.Scalaz.Id
import utest._

object SourceTest extends TestSuite {

  def s1: Source[Id] = Source.environment
  def ss: Sources[Id] = s1

  def ensureCompilation(s: => Sources[Id]) = ()

  override def tests = TestSuite {

    "1>n" - ensureCompilation(s1 > ss)
    "1<n" - ensureCompilation(s1 < ss)
    "n>1" - ensureCompilation(ss > s1)
    "n<1" - ensureCompilation(ss < s1)
    "1>1" - ensureCompilation(s1 > s1)
    "1<1" - ensureCompilation(s1 < s1)
    "n>n" - ensureCompilation(ss > ss)
    "n<n" - ensureCompilation(ss < ss)

    "chain" - ensureCompilation {
      val sources: List[Source[Id]] = null
      Source.environment[Id] > Sources.highToLowPri(sources: _*) > Source.system[Id]
    }
  }
}
