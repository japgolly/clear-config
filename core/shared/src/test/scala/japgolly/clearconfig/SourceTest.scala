package japgolly.clearconfig

import cats.Id
import utest._

object SourceTest extends TestSuite {

  def s1: ConfigSource[Id] = ConfigSource.empty("S1")
  def ss: ConfigSources[Id] = s1

  def ensureCompilation(s: => ConfigSources[Id]) = ()

  override def tests = Tests {

    "1>n" - ensureCompilation(s1 > ss)
    "1<n" - ensureCompilation(s1 < ss)
    "n>1" - ensureCompilation(ss > s1)
    "n<1" - ensureCompilation(ss < s1)
    "1>1" - ensureCompilation(s1 > s1)
    "1<1" - ensureCompilation(s1 < s1)
    "n>n" - ensureCompilation(ss > ss)
    "n<n" - ensureCompilation(ss < ss)

    "chain" - ensureCompilation {
      val sources: List[ConfigSource[Id]] = null
      ConfigSource.empty[Id]("H") > ConfigSources.highToLowPri(sources: _*) > ConfigSource.empty[Id]("L")
    }
  }
}
