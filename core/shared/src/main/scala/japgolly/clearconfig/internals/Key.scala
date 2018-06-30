package japgolly.clearconfig.internals

final case class Key(value: String) {

  def map(f: String => String): Key =
    Key(f(value))

  def toUpperCase                                  : Key = map(_.toUpperCase)
  def toLowerCase                                  : Key = map(_.toLowerCase)
  def replace(from: Char, to: Char)                : Key = map(_.replace(from, to))
  def replace(from: CharSequence, to: CharSequence): Key = map(_.replace(from, to))
}
