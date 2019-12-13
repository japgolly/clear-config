import sbt._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{crossProject => _, CrossType => _, _}
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object ClearConfig {

  private val ghProject = "clearconfig"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    val JavaTimeScalaJs = "0.2.6"
    val KindProjector   = "0.10.3"
    val Microlibs       = "2.0-RC1"
    val MTest           = "0.7.1"
    val Scala212        = "2.12.10"
    val Scala213        = "2.13.1"
    val ScalaCollCompat = "2.1.3"
    val Scalaz          = "7.2.29"
  }

  def scalacFlags =
    Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-opt:l:inline",
      "-opt-inline-from:japgolly.clearconfig.**",
      "-Ywarn-dead-code",
      "-Ywarn-unused",
      "-Ywarn-value-discard")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.clearconfig",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code", "-Ywarn-unused"),
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("org.typelevel" %% "kind-projector" % Ver.KindProjector)))

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % Test,
      testFrameworks      += new TestFramework("utest.runner.Framework")))

  // ===================================================================================================================

  lazy val root =
    Project("root", file("."))
      .settings(name := "ClearConfig")
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(coreJVM, coreJS)

  lazy val coreJVM = core.jvm
  lazy val coreJS  = core.js
  lazy val core = crossProject(JVMPlatform, JSPlatform)
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang.modules"        %%% "scala-collection-compat" % Ver.ScalaCollCompat,
        "org.scalaz"                    %%% "scalaz-core"             % Ver.Scalaz,
        "com.github.japgolly.microlibs" %%% "stdlib-ext"              % Ver.Microlibs,
        "com.github.japgolly.microlibs" %%% "test-util"               % Ver.Microlibs % Test,
        "com.github.japgolly.microlibs" %%% "utils"                   % Ver.Microlibs))
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-java-time" % Ver.JavaTimeScalaJs % Test))
}
