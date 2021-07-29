import sbt._
import sbt.Keys._
import com.jsuereth.sbtpgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Dependencies._
import Lib._

object ClearConfig {

  private val ghProject = "clearconfig"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  def scalacCommonFlags: Seq[String] = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
  )

  def scalac2Flags = Seq(
    "-opt:l:inline",
    "-opt-inline-from:japgolly.clearconfig.**",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
  )

  def scalac3Flags = Seq(
    "-source:3.0-migration",
    "-Ykind-projector",
  )

  val commonSettings = ConfigureBoth(
    _.settings(
      scalaVersion                  := Ver.scala2,
      crossScalaVersions            := Seq(Ver.scala2),
      scalacOptions                ++= scalacCommonFlags,
      scalacOptions                ++= byScalaVersion {
                                         case (2, _) => scalac2Flags
                                         case (3, _) => scalac3Flags
                                       }.value,
      Test / scalacOptions         --= Seq("-Ywarn-dead-code"),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(ThisBuild / version).value}",
      releaseVcsSign                := true,
      libraryDependencies          ++= Seq(Dep.betterMonadicFor, Dep.kindProjector).filter(_ => scalaVersion.value startsWith "2"),
    )
  )

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += Dep.utest.value % Test,
      testFrameworks      += new TestFramework("utest.runner.Framework"),
    )
  )

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
        Dep.cats              .value,
        Dep.microlibsStdlibExt.value,
        Dep.microlibsUtils    .value,
        Dep.microlibsTestUtil .value % Test,
      ),
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        Dep.scalaJsJavaTime.value % Test,
      ),
    )
}
