import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import pl.project13.scala.sbt.JmhPlugin
import Lib._

object PondBuild {

  private val ghProject = "pond-scala"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val JAMM          = "0.3.1"
    final val KindProjector = "0.9.3"
    final val MacroParadise = "2.1.0"
    final val Monocle       = "1.3.2"
    final val MTest         = "0.4.4"
    final val Scala211      = "2.11.8"
    final val Scala212      = "2.12.0"
    final val Scalaz        = "7.2.7"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials")

  val commonSettings = ConfigureBoth(
    _.settings(
      version                  := "unspecified-SNAPSHOT",
      organization             := "com.github.japgolly.pond",
      homepage                 := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                 += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion             := Ver.Scala211,
      crossScalaVersions       := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions           ++= scalacFlags,
      scalacOptions in Test   --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage         := Watched.clearWhenTriggered,
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector))
    .configure(
      addCommandAliases(
        "/"   -> "project root",
        "L"   -> "root/publishLocal",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "TL"  -> ";T;L",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "to"  -> "test-only",
        "tq"  -> "test-quick",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test")))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % "test",
      testFrameworks      += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      // Not mandatory; just faster.
      _.settings(jsEnv in Test := PhantomJSEnv().value))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        recursionJVM, recursionJS,
        bench)

  lazy val recursionJVM = recursion.jvm
  lazy val recursionJS  = recursion.js
  lazy val recursion = crossProject
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      version := "1.0.0-SNAPSHOT",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val bench = project.in(file("bench"))
    .dependsOn(recursionJVM % "compile->test")
    .enablePlugins(JmhPlugin)
    .configure(commonSettings.jvm, preventPublication)
    .settings(
      name := "bench",
      libraryDependencies += "com.github.jbellis" % "jamm" % Ver.JAMM,
      fork := true,
      javaOptions ++= Seq("-server", "-Xss8M"),

      // Add the JAMM jar as an agent
      javaOptions in run := {
        val classPath = (dependencyClasspath in Compile).value
        val jammJar = classPath.collectFirst {
          case sbt.Attributed(f) if f.getName.matches("jamm-[0-9.]+\\.jar") => f.getAbsolutePath
        }.get
        val oldOptions = (javaOptions in run).value
        val newOptions = oldOptions :+ s"-javaagent:$jammJar"
        newOptions
      }
    )
}
