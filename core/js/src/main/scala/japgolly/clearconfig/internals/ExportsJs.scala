package japgolly.clearconfig.internals

trait ExportsJs extends Exports {

  override final type ConfigStoreObject = StoreObjectJs
  override final val ConfigStore = StoreJs

  override final type ConfigSourceObject = SourceObjectJs
  override final val ConfigSource = SourceJs

  override final type ConfigSourceNameObject = SourceNameObjectJs
  override final val ConfigSourceName = SourceNameJs

}