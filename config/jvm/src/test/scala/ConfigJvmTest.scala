package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.Equal
import scalaz.Scalaz.Id
import utest._
import ConfigParser.Implicits.Defaults._
import java.io.File
import java.nio.file.Files
import java.util.UUID

object ConfigJvmTest extends TestSuite {

  override def tests = TestSuite {

    'propFileOnClasspath {

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

    'propFileOnFS - {
      val tmpFile = new File(s"/tmp/config-test-${UUID.randomUUID()}.props")
      tmpFile.deleteOnExit()

      'found - {
        val content = "a=172".getBytes
        Files.write(tmpFile.toPath, content)
        val src = Source.propFile[Id](tmpFile.getAbsolutePath, optional = false)
        val r = Config.get[Int]("a").run(src)
        assertEq(r, ConfigResult.Success(Option(172)))(Equal.equalA)
      }

      'notFoundMandatory - {
        val src = Source.propFile[Id](tmpFile.getAbsolutePath, optional = false)
        val r = Config.get[String]("x").run(src)
        assertEq(r, ConfigResult.PreparationFailure(src.name, "File not found."))(Equal.equalA)
      }

      'notFoundOptional - {
        val src = Source.propFile[Id](tmpFile.getAbsolutePath, optional = true)
        val r = Config.get[String]("x").run(src)
        assertEq(r, ConfigResult.Success(None))(Equal.equalA)
      }

    }

  }
}
