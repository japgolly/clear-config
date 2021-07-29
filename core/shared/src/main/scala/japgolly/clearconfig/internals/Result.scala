package japgolly.clearconfig.internals

sealed abstract class Result[+A] {
  def toEither: Either[String, A]

  def getOrDie(): A =
    toEither match {
      case Right(a) => a
      case Left(e) => sys error e
    }
}

object Result {

  final case class Success[+A](value: A) extends Result[A] {
    override def toEither = Right(value)
  }

  final case class PreparationFailure(sourceName: SourceName, error: String) extends Result[Nothing] {
    def errorMsg: String = s"Error preparing source [$sourceName]: $error"
    override def toEither = Left(errorMsg)
  }

  final case class QueryFailure(keyed: Map[Key, Option[(SourceName, Lookup.Error)]],
                                other: Set[(String, Set[Either[String, Key]])],
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
          val constituents = s1.toList.map(_.fold("" + _, fmtKey)).sorted.mkString(", ")
          s"Error using $constituents: $desc"
      }
      val errors = (eachK ++ eachO).sorted

      s"""
         |${Util.fmtList("error", "errors", errors)}
         |
         |${Util.fmtSourceNameList(sourcesHighToLowPri)}
       """.stripMargin.trim
    }
    override def toEither = Left(errorMsg)
  }
}
