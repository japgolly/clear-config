package japgolly.microlibs.name_fn

abstract class Name {
  def value: String
  def map(f: String => String): Name
}

object Name {

  final class Now(override val value: String) extends Name {
    override def map(f: String => String): Name =
      now(f(value))
  }

  final class Later(init: () => String) extends Name {
    private[this] var thunk = init

    override lazy val value: String = {
      val n = thunk()
      thunk = null // dereference
      n
    }

    override def map(f: String => String): Name =
      Name(f(value))
  }

  def now(value: String): Now =
    new Now(value)

  def apply(n: => String): Name =
    new Later(() => n)

  def lazily(n: => Name): Name =
    new Later(() => n.value)

  object Implicits extends NameImplicits
}
