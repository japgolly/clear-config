package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.Equal
import scalaz.Scalaz.Id
import utest._
import ConfigParser.Implicits.Defaults._

object ConfigJvmTest extends TestSuite {

  override def tests = TestSuite {

    'propFiles {

      'notFoundMandatory - {
        val src = Source.propFileOnClasspath[Id]("what.props", optional = false)
        val r = Config.get[String]("x").run(src)
        assertEq(r, ConfigResult.PreparationFailure(src.name, "File not found."))(Equal.equalA)
      }

      'notFoundOptional - {
        val r = Config.get[String]("x").run(Source.propFileOnClasspath[Id]("what.props", optional = true))
        assertEq(r, ConfigResult.Success(None))(Equal.equalA)
      }

      'found - {
        val r = Config.need[String]("from.file.2").run(Source.propFileOnClasspath[Id]("blah.props", optional = false))
        assertEq(r, ConfigResult.Success("really good"))(Equal.equalA)
      }

    }

  }
}
