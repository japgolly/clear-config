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
    val JavaTimeScalaJs = "0.2.4"
    val KindProjector   = "0.9.7"
    val Microlibs       = "1.16"
    val MTest           = "0.5.4"
    val Scala211        = "2.11.12"
    val Scala212        = "2.12.6"
    val Scalaz          = "7.2.24"
  }

  def scalacFlags = Def.setting(
    Seq(
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
    ++ (scalaVersion.value match {
      case x if x startsWith "2.11." => "-target:jvm-1.6" :: Nil
      case x if x startsWith "2.12." => "-target:jvm-1.8" :: "-opt:l:method" :: Nil
    }))

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.clearconfig",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala212,
      crossScalaVersions            := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions                ++= scalacFlags.value,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code", "-Ywarn-unused"),
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage              := Watched.clearWhenTriggered,
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector)))

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
        "org.scalaz"                    %%% "scalaz-core" % Ver.Scalaz,
        "com.github.japgolly.microlibs" %%% "stdlib-ext"  % Ver.Microlibs,
        "com.github.japgolly.microlibs" %%% "test-util"   % Ver.Microlibs % Test,
        "com.github.japgolly.microlibs" %%% "utils"       % Ver.Microlibs))
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-java-time" % Ver.JavaTimeScalaJs % Test))
}
