import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Ver {

    // Externally observable
    val microlibs             = "2.6"
    val scala2                = "2.13.6"
    val scalaz                = "7.2.30"

    // Internal
    val betterMonadicFor      = "0.3.1"
    val kindProjector         = "0.13.0"
    val scalaJsJavaTime       = "1.0.0"
    val utest                 = "0.7.10"
  }

  object Dep {
    val microlibsStdlibExt   = Def.setting("com.github.japgolly.microlibs" %%% "stdlib-ext"        % Ver.microlibs)
    val microlibsTestUtil    = Def.setting("com.github.japgolly.microlibs" %%% "test-util"         % Ver.microlibs)
    val microlibsUtils       = Def.setting("com.github.japgolly.microlibs" %%% "utils"             % Ver.microlibs)
    val scalaJsJavaTime      = Def.setting("org.scala-js"                  %%% "scalajs-java-time" % Ver.scalaJsJavaTime cross CrossVersion.for3Use2_13)
    val scalaz               = Def.setting("org.scalaz"                    %%% "scalaz-core"       % Ver.scalaz)
    val utest                = Def.setting("com.lihaoyi"                   %%% "utest"             % Ver.utest)

    // Compiler plugins
    val betterMonadicFor = compilerPlugin("com.olegpy"     %% "better-monadic-for" % Ver.betterMonadicFor)
    val kindProjector    = compilerPlugin("org.typelevel"  %% "kind-projector"     % Ver.kindProjector cross CrossVersion.full)
  }
}
