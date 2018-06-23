package japgolly.clearconfig.internals

trait Exports extends Implicits {

  final type ConfigStore[F[_]] = japgolly.clearconfig.internals.Store[F]
  type ConfigStoreObject <: japgolly.clearconfig.internals.StoreObject
  val ConfigStore: ConfigStoreObject

  final type ConfigSource[F[_]] = japgolly.clearconfig.internals.Source[F]
  type ConfigSourceObject <: japgolly.clearconfig.internals.SourceObject
  val ConfigSource: ConfigSourceObject

  final type ConfigSourceName = japgolly.clearconfig.internals.SourceName
  type ConfigSourceNameObject <: japgolly.clearconfig.internals.SourceNameObject
  val ConfigSourceName: ConfigSourceNameObject

  final type ConfigSources[F[_]] = japgolly.clearconfig.internals.Sources[F]
  final val  ConfigSources       = japgolly.clearconfig.internals.Sources

  final type ConfigValueParser[A] = japgolly.clearconfig.internals.ValueParser[A]
  final val  ConfigValueParser    = japgolly.clearconfig.internals.ValueParser

  final type ConfigValuePreprocessor = japgolly.clearconfig.internals.ValuePreprocessor
  final val  ConfigValuePreprocessor = japgolly.clearconfig.internals.ValuePreprocessor

  final type ConfigDef[A] = japgolly.clearconfig.internals.Config[A]
  final val  ConfigDef    = japgolly.clearconfig.internals.Config

  final type ConfigResult[+A] = japgolly.clearconfig.internals.Result[A]
  final val  ConfigResult     = japgolly.clearconfig.internals.Result

  final type ConfigReport = japgolly.clearconfig.internals.Report
  final val  ConfigReport = japgolly.clearconfig.internals.Report

}
