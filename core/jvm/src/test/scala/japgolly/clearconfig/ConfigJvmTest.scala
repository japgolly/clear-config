package japgolly.clearconfig

import japgolly.microlibs.testutil.TestUtil._
import java.io.File
import java.nio.file.Files
import java.util.UUID
import cats.Id
import cats.implicits._
import utest._

object ConfigJvmTest extends TestSuite {

  override def tests = Tests {

    "environment" - {
      val s = ConfigSource.environment[Id]
      println(ConfigDef.get[String]("JAVA.HOME").withReport.run(s).getOrDie()._2.obfuscateKeys(_.value.toUpperCase.contains("PASS")).full)
    }

    "propFileOnClasspath" - {

      "notFoundMandatory" - {
        val src = ConfigSource.propFileOnClasspath[Id]("what.props", optional = false)
        val r = ConfigDef.get[String]("x").run(src)
        assertEq(r, ConfigResult.PreparationFailure(src.name, "File not found."))(Eq.equalA, implicitly)
      }

      "notFoundOptional" - {
        val r = ConfigDef.get[String]("x").run(ConfigSource.propFileOnClasspath[Id]("what.props", optional = true))
        assertEq(r, ConfigResult.Success(None))(Eq.equalA, implicitly)
      }

      "found" - {
        val r = ConfigDef.need[String]("from.file.2").run(ConfigSource.propFileOnClasspath[Id]("blah.props", optional = false))
        assertEq(r, ConfigResult.Success("really good"))(Eq.equalA, implicitly)
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
        assertEq(r, ConfigResult.Success(Option(172)))(Eq.equalA, implicitly)
      }

      "notFoundMandatory" - {
        val src = ConfigSource.propFile[Id](tmpFile.getAbsolutePath, optional = false)
        val r = ConfigDef.get[String]("x").run(src)
        assertEq(r, ConfigResult.PreparationFailure(src.name, "File not found."))(Eq.equalA, implicitly)
      }

      "notFoundOptional" - {
        val src = ConfigSource.propFile[Id](tmpFile.getAbsolutePath, optional = true)
        val r = ConfigDef.get[String]("x").run(src)
        assertEq(r, ConfigResult.Success(None))(Eq.equalA, implicitly)
      }

    }

    "inlineProps" - {

      "ok" - {
        val src1 = ConfigSource.manual[Id]("a")(
          "x.1" -> "hehe",
          "INLINE" ->
            s"""
               |# hehe
               | x.2      = 123
               | x.3    = good stuff # nice
               |""".stripMargin
        )
        val src = ConfigSource.expandInlineProperties(src1, "INLINE")

        val cfgDef = (
          ConfigDef.need[String]("x.1") |@|
            ConfigDef.need[String]("x.2") |@|
            ConfigDef.need[String]("x.3")
          ) ((_, _, _)).withReport

        val (xs, report) = cfgDef.run(src).getOrDie()
        assert(xs == (("hehe", "123", "good stuff")))
        assert(!report.full.contains("INLINE"))
        report.full
      }

      "ko" - {
        val src1 = ConfigSource.manual[Id]("a")(
          "x.1" -> "hehe",
          "INLINE" ->
            s"""
               |# hehe
               | x.1      = 123
               |""".stripMargin
        )
        val src = ConfigSource.expandInlineProperties(src1, "INLINE")

        val cfgDef = ConfigDef.need[String]("x.1")

        val result = cfgDef.run(src).toEither
        assert(result == Left("Error preparing source [SourceName(a)]: The following keys are defined at both the top-level and in INLINE: x.1."))
      }
    }

  }
}
