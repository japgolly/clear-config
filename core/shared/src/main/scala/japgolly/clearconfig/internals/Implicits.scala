package japgolly.clearconfig.internals

trait Implicits extends ValueParser.Implicits {

  implicit def configSourceToSources[F[_]](s: Source[F]): Sources[F] =
    s.toSources

  implicit def configSeqSourceToSeqSources[F[_]](s: Seq[Source[F]]): Seq[Sources[F]] =
    s.map(_.toSources)

  implicit def configValuePreprocessor: ValuePreprocessor =
    ValuePreprocessor.default

  implicit def configReportSettings: Report.Settings =
    Report.Settings.default
}
