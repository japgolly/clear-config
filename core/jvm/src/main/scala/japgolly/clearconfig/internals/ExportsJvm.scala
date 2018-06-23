package japgolly.clearconfig.internals

trait ExportsJvm extends Exports {

  override final type ConfigStoreObject = StoreObjectJvm
  override final val ConfigStore = StoreJvm

  override final type ConfigSourceObject = SourceObjectJvm
  override final val ConfigSource = SourceJvm

  override final type ConfigSourceNameObject = SourceNameObjectJvm
  override final val ConfigSourceName = SourceNameJvm

}