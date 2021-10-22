ThisBuild / homepage      := Some(url("https://github.com/japgolly/clearconfig"))
ThisBuild / licenses      := ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")) :: Nil
ThisBuild / organization  := "com.github.japgolly.clearconfig"
ThisBuild / shellPrompt   := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / startYear     := Some(2016)
ThisBuild / versionScheme := Some("early-semver")
sonatypeProfileName       := "com.github.japgolly"

val root    = ClearConfig.root
val coreJVM = ClearConfig.coreJVM
val coreJS  = ClearConfig.coreJS
