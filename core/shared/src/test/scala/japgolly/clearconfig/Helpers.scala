package japgolly.clearconfig

import cats.{Eq, Id}
import japgolly.microlibs.testutil.TestUtil._

object Helpers extends internals.Exports {
  type Key = internals.Key
  val  Key = internals.Key
  type Lookup = internals.Lookup
  val  Lookup = internals.Lookup

  override type ConfigStoreObject = internals.StoreObject
  override val ConfigStore = internals.Platform.ConfigStore

  override type ConfigSourceObject = internals.SourceObject
  override val ConfigSource: ConfigSourceObject = internals.Platform.ConfigSource

  override type ConfigSourceNameObject = internals.SourceNameObject
  override val ConfigSourceName = internals.SourceName

  implicit def equalResultX[A]: Eq[ConfigResult[A]] = Eq.fromUniversalEquals
  implicit def equalURL       : Eq[java.net.URL]    = Eq.fromUniversalEquals

  val src0 = ConfigSource.manual[Id]("S0")()
  val src1 = ConfigSource.manual[Id]("S1")("in" -> "100", "i" -> "3", "s" -> "hey", "dur3m" -> "3 min")
  val src2 = ConfigSource.manual[Id]("S2")("in" -> "200", "i" -> "X300", "i2" -> "22", "s2" -> "ah")

  val srcs: ConfigSources[Id] = src1 > src2
  val srcNames = srcs.highToLowPri.map(_.name)

  val srcE = ConfigSource.point[Id]("SE",
    ConfigStore[Id](Map.empty[Key, String], (_: Key) => Lookup.Error("This source is fake!", None)))

  implicit class ResultXExt[A](private val self: ConfigResult[A]) extends AnyVal {
    def get_! : A = self match {
      case ConfigResult.Success(a) => a
      case x => fail(s"Expected success, got: $x")
    }
  }

}
