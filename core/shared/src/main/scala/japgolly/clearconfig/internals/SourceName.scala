package japgolly.clearconfig.internals

final case class SourceName(value: String) extends AnyVal {
  def withSuffix(s: String): SourceName =
    SourceName(value + s)
}

trait SourceNameObject {

  final def default = SourceName("Default")

}

object SourceName extends SourceNameObject