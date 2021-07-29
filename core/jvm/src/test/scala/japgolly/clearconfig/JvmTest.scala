package japgolly.clearconfig

import cats.{Eq, Id}
import japgolly.microlibs.testutil.TestUtil._
import java.io.File
import java.nio.file.Files
import java.util.UUID
import utest._

object JvmTest extends TestSuite {

  override def tests = Tests {

    "propFileOnClasspath" - {

      "notFoundMandatory" - {
        val src = ConfigSource.propFileOnClasspath[Id]("what.props", optional = false)
        val r = ConfigDef.get[String]("x").run(src)
        assertEq(r, ConfigResult.PreparationFailure(src.name, "File not found."))(Eq.fromUniversalEquals, implicitly)
      }

      "notFoundOptional" - {
        val r = ConfigDef.get[String]("x").run(ConfigSource.propFileOnClasspath[Id]("what.props", optional = true))
        assertEq(r, ConfigResult.Success(None))(Eq.fromUniversalEquals, implicitly)
      }

      "found" - {
        val r = ConfigDef.need[String]("from.file.2").run(ConfigSource.propFileOnClasspath[Id]("blah.props", optional = false))
        assertEq(r, ConfigResult.Success("really good"))(Eq.fromUniversalEquals, implicitly)
      }

    }

    "propFileOnFS" - {
      val tmpFile = new File(s"/tmp/config-test-${UUID.randomUUID()}.props")
      tmpFile.deleteOnExit()

      "found" - {
        val content = "a=172".getBytes
        Files.write(tmpFile.toPath, content)
        val src = ConfigSource.propFile[Id](tmpFile.getAbsolutePath, optional = false)
        val r = ConfigDef.get[Int]("a").run(src)
        assertEq(r, ConfigResult.Success(Option(172)))(Eq.fromUniversalEquals, implicitly)
      }

      "notFoundMandatory" - {
        val src = ConfigSource.propFile[Id](tmpFile.getAbsolutePath, optional = false)
        val r = ConfigDef.get[String]("x").run(src)
        assertEq(r, ConfigResult.PreparationFailure(src.name, "File not found."))(Eq.fromUniversalEquals, implicitly)
      }

      "notFoundOptional" - {
        val src = ConfigSource.propFile[Id](tmpFile.getAbsolutePath, optional = true)
        val r = ConfigDef.get[String]("x").run(src)
        assertEq(r, ConfigResult.Success(None))(Eq.fromUniversalEquals, implicitly)
      }

    }

  }
}
