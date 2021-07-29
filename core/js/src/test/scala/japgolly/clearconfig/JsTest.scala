package japgolly.clearconfig

import cats.Id
import utest._

object JsTest extends TestSuite {

  override def tests = Tests {

    "env" - {
      val s = ConfigSource.environment[Id]
      val v = ConfigDef.get[String]("PATH").run(s).getOrDie()
      assert(v.isDefined)
    }

  }
}
