package japgolly.clearconfig.internals

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz._
import Scalaz._
import Evaluation._

private[internals] object ReportCreation {

  private def sourceName = SourceName.default

  def apply[F[_]](r: R[F], s: S[F])(implicit F: Applicative[F]): F[Report] = {
    type M = Map[Key, Map[SourceName, Lookup]]
    def emptyM: M = Map.empty
    implicit def semigroupLookup: Semigroup[Lookup] =
      Semigroup.firstSemigroup // There will never be overlap

    val defaultProps: Option[Map[Key, String]] = {
      val m: Map[Key, String] =
        s.apiData
          .mapOrRemoveValues(_.toList match {
            case Nil                          => None
            case ApiMethod.Get         :: Nil => None
            case ApiMethod.Need        :: Nil => None
            case ApiMethod.GetOrUse(u) :: Nil => Some(u)
            case xs@(_ :: _ :: _) => Some(xs.map {
              case ApiMethod.Get         => "<get>"
              case ApiMethod.Need        => "<need>"
              case ApiMethod.GetOrUse(u) => u
            }.sorted.mkString(" / "))
          })
      if (m.isEmpty) None else Some(m)
    }

    var srcs = r.highToLowPri.map(_._1)

    var fUsed: F[M] =
      s.queryCache
        .toVector
        .traverse { case (k, vof) =>
          vof.highToLowPri.map(_.flatMap(sv =>
            sv.value match {
              case Lookup.Found(k2, _) if k2 != k =>
                Vector((k2, sv.source, sv.value), (k, sv.source, Lookup.NotFound))
              case _ =>
                Vector((k, sv.source, sv.value))
            }
          ))
        }
        .map(_.iterator
          .flatMap(_.map { case (k, s, v) => emptyM.updated(k, Map(s -> v)) })
          .foldLeft(emptyM)(_ |+| _))

    for (ap <- defaultProps) {
      srcs :+= sourceName
      fUsed = fUsed.map { used =>
        ap.foldLeft(used){ case (q, (k, v)) =>
          q.initAndModifyValue(k, Map.empty, _.updated(sourceName, Lookup.Found(k, v)))
        }
      }
    }

    val usedKeys: Set[Key] =
      s.queryCache.keySet

    val fProbablyUnused: F[M] =
      r.highToLowPri.traverse { case (src, store) =>
        store.getBulk(!usedKeys.contains(_))
          .map(_.map(kv => kv._1 -> Map(src -> Lookup.Found(kv._1, kv._2))))
      }.map(_.foldLeft(emptyM)(_ |+| _))

    val fReport: F[Report] =
      F.apply2(fUsed, fProbablyUnused) { (used, probablyUnused) =>
        val unused: M =
          used.foldLeft(probablyUnused) { case (m, (k, usedValues)) =>
            m.modifyValueOption(k, _.map(_.filterKeys(s => !usedValues.contains(s))).filter(_.nonEmpty))
          }

        val keysToObfuscate: Set[Key] =
          s.keysToObfuscate.map(_.toLowerCase)

        Report.withDefaults(srcs, used, unused)
          .obfuscateKeys(k => keysToObfuscate.contains(k.toLowerCase))
      }

    fReport
  }

}
