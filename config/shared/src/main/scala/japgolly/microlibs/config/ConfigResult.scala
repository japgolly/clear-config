package japgolly.microlibs.config

import scalaz.{-\/, \/, \/-}

sealed abstract class ConfigResult[+A] {
  def toDisjunction: String \/ A

  def getOrDie(): A =
    toDisjunction match {
      case \/-(a) => a
      case -\/(e) => sys error e
    }
}

object ConfigResult {
  final case class PreparationFailure(sourceName: SourceName, error: String) extends ConfigResult[Nothing] {
    def errorMsg: String = s"Error preparing source [$sourceName]: $error"
    override def toDisjunction = -\/(errorMsg)
  }

  final case class QueryFailure(keyed: Map[Key, Option[(SourceName, ConfigValue.Error)]],
                                other: Set[(String, Set[String \/ Key])]) extends ConfigResult[Nothing] {
    def errorMsg: String = {
      def fmtKey(k: Key) = s"key [${k.value}]"
      val eachK = keyed.toVector.map {
        case (k, None) =>
          s"No value for ${fmtKey(k)}"
        case (k, Some((SourceName(src), ConfigValue.Error(desc, None)))) =>
          s"Error reading ${fmtKey(k)} from source [$src]: $desc"
        case (k, Some((SourceName(src), ConfigValue.Error(desc, Some(v))))) =>
          s"Error reading ${fmtKey(k)} from source [$src] with value [$v]: $desc"
      }
      val eachO = other.toVector.map {
        case (desc, s1) =>
          val constituents = s1.toList.map(_.fold(_.toString, fmtKey)).sorted.mkString(", ")
          s"Error using $constituents: $desc"
      }
      var errors = "error"
      val each = eachK ++ eachO
      if (each.length != 1) errors += "s"
      s"${each.length} $errors:${each.sorted.map("\n  - " + _).mkString}"
    }
    override def toDisjunction = -\/(errorMsg)
  }

  final case class Success[+A](value: A) extends ConfigResult[A] {
    override def toDisjunction = \/-(value)
  }
}
