package japgolly.clearconfig.internals

final case class SourceName(value: String) extends AnyVal

trait SourceNameObject {

  final def default = SourceName("Default")

}

object SourceName extends SourceNameObject