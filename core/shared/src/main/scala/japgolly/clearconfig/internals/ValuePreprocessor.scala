package japgolly.clearconfig.internals

final case class ValuePreprocessor(run: String => String) extends AnyVal {
  def andThen(next: ValuePreprocessor): ValuePreprocessor =
    ValuePreprocessor(run andThen next.run)
}

object ValuePreprocessor {

  def id: ValuePreprocessor =
    apply(identity)

  def trim: ValuePreprocessor =
    apply(_.trim)

  def takeUntil(token: String): ValuePreprocessor =
    apply { s =>
      val n = s.indexOf(token)
      if (n >= 0)
        s.take(n)
      else
        s
    }

  def removeComment(token: String): ValuePreprocessor =
    apply { s =>
      if (s.startsWith(token))
        ""
      else
        takeUntil(" " + token).run(s)
    }

  def removeHashComments: ValuePreprocessor =
    removeComment("#")

  def default: ValuePreprocessor =
    removeHashComments andThen trim
}
