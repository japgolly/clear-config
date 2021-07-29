package japgolly.clearconfig.internals

final case class SourceName(value: String) extends AnyVal {
  def withSuffix(s: String): SourceName =
    SourceName(value + s)
}

trait SourceNameObject {

  final def apply(name: String) = new SourceName(name)
  final def default = SourceName("Default")
  final def environment = SourceName("Env")

}

object SourceName extends SourceNameObject