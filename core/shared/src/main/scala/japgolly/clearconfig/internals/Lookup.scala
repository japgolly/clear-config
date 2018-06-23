package japgolly.clearconfig.internals

sealed trait Lookup extends Product with Serializable

object Lookup {

  case object NotFound extends Lookup
  final case class Found(key: Key, value: String) extends Lookup
  final case class Error(desc: String, value: Option[String]) extends Lookup

  @inline def notFound: Lookup = NotFound

  def fromOption(k: Key, o: Option[String]): Lookup =
    o match {
      case Some(v) => Found(k, v)
      case None    => NotFound
    }
}

