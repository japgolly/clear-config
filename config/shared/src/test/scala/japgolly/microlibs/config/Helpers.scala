package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.Scalaz.Id

object Helpers {

  implicit def equalResultX[A] = scalaz.Equal.equalA[ConfigResult[A]]

  val src1 = Source.manual[Id]("S1")("in" -> "100", "i" -> "3", "s" -> "hey", "dur3m" -> "3 min")
  val src2 = Source.manual[Id]("S2")("in" -> "200", "i" -> "X300", "i2" -> "22", "s2" -> "ah")

  val srcs: Sources[Id] =
     src1 > src2

  val srcE = Source.point[Id]("SE", new ConfigStore[Id] {
    override def apply(key: Key) = ConfigValue.Error("This source is fake!", None)
    override def getBulk(f: Key => Boolean) = Map.empty
  })

  implicit class ResultXExt[A](private val self: ConfigResult[A]) extends AnyVal {
    def get_! : A = self match {
      case ConfigResult.Success(a) => a
      case x => fail(s"Expected success, got: $x")
    }
  }

}
