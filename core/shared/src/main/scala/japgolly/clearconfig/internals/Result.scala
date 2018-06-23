package japgolly.clearconfig.internals

import scalaz.{-\/, \/, \/-}

sealed abstract class Result[+A] {
  def toDisjunction: String \/ A

  def toEither: Either[String, A] =
    toDisjunction.toEither

  def getOrDie(): A =
    toDisjunction match {
      case \/-(a) => a
      case -\/(e) => sys error e
    }
}

object Result {

  final case class Success[+A](value: A) extends Result[A] {
    override def toDisjunction = \/-(value)
  }

  final case class PreparationFailure(sourceName: SourceName, error: String) extends Result[Nothing] {
    def errorMsg: String = s"Error preparing source [$sourceName]: $error"
    override def toDisjunction = -\/(errorMsg)
  }

  final case class QueryFailure(keyed: Map[Key, Option[(SourceName, Lookup.Error)]],
                                other: Set[(String, Set[String \/ Key])],
                                sourcesHighToLowPri: Vector[SourceName]) extends Result[Nothing] {
    def errorMsg: String = {
      def fmtKey(k: Key) = s"key [${k.value}]"
      val eachK = keyed.toVector.map {
        case (k, None) =>
          s"No value for ${fmtKey(k)}"
        case (k, Some((SourceName(src), Lookup.Error(desc, None)))) =>
          s"Error reading ${fmtKey(k)} from source [$src]: $desc"
        case (k, Some((SourceName(src), Lookup.Error(desc, Some(v))))) =>
          s"Error reading ${fmtKey(k)} from source [$src] with value [$v]: $desc"
      }
      val eachO = other.toVector.map {
        case (desc, s1) =>
          val constituents = s1.toList.map(_.fold(_.toString, fmtKey)).sorted.mkString(", ")
          s"Error using $constituents: $desc"
      }
      val errors = (eachK ++ eachO).sorted

      s"""
         |${Util.fmtList("error", "errors", errors)}
         |
         |${Util.fmtSourceNameList(sourcesHighToLowPri)}
       """.stripMargin.trim
    }
    override def toDisjunction = -\/(errorMsg)
  }
}
