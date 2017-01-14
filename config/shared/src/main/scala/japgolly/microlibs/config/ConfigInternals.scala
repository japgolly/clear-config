package japgolly.microlibs.config

import scalaz._
import Scalaz._
import japgolly.microlibs.stdlib_ext.StdlibExt._

private[config] object ConfigInternals {

  type Step[F[_], A] = RWS[R[F], Unit, S[F], F[A]]

  final case class R[F[_]](highToLowPri: Vector[(SourceName, ConfigStore[F])])

  final case class S[F[_]](keyModStack: List[Key => Key], queryCache: Map[Key, VO[F]], apiData: Map[Key, Set[ApiMethod]]) {
    def keyMod: Key => Key =
      keyModStack.headOption.getOrElse(identity[Key])
    def keyModPush(f: Key => Key): S[F] =
      copy(f :: keyModStack)
    def keyModPop: S[F] =
      keyModStack match {
        case Nil => this
        case _ :: t => copy(t)
      }
    def addApi(key: Key, apiMethod: ApiMethod): S[F] =
      copy(apiData = apiData.initAndModifyValue(key, Set.empty, _ + apiMethod))
  }
  object S {
    def init[F[_]]: S[F] = S(Nil, Map.empty, Map.empty)
  }

  case class SV(source: SourceName, value: ConfigValue)
  case class VO[F[_]](highToLowPri: F[Vector[SV]], selected: F[Option[SV]])

  sealed abstract class Origin
  object Origin {
    final case class Point(value: () => String) extends Origin
    final case class Read(key: Key, sourceName: SourceName, sourceValue: ConfigValue.Found) extends Origin
    case object Map extends Origin
  }

  final case class UnkeyedError(error: String, origins: Set[Origin]) {
    def public: (String, Set[String \/ Key]) =
      (error, origins.map {
        case Origin.Map => -\/("<function>")
        case Origin.Point(f) =>
          -\/(f() match {
            case s if s.matches("^<function\\d+>$") | s.contains("$Lambda$") => "<function>"
            case s => s"runtime value [$s]"
          })
        case Origin.Read(k, _, _) => \/-(k)
      })
  }

  sealed abstract class StepResult[+A] {
    def map[B](f: A => B): StepResult[B]
    def flatMap[B](f: A => StepResult[B]): StepResult[B]
    def addOrigins(o: Set[Origin]): StepResult[A]
  }

  object StepResult {

    final case class Failure(keyed: Map[Key, Option[(SourceName, ConfigValue.Error)]],
                             other: Set[UnkeyedError]) extends StepResult[Nothing] {
      override def map[B](f: Nothing => B): StepResult[B] = this
      override def flatMap[B](f: Nothing => StepResult[B]): StepResult[B] = this
      override def addOrigins(o: Set[Origin]) = this
    }

    final case class Success[+A](value: A, origins: Set[Origin]) extends StepResult[A] {
      override def map[B](f: A => B): StepResult[B] = Success(f(value), origins)
      override def flatMap[B](f: A => StepResult[B]): StepResult[B] = f(value) addOrigins origins
      override def addOrigins(o: Set[Origin]) = Success(value, origins ++ o)
    }

    implicit val applicativeInstance: Applicative[StepResult] =
      new Applicative[StepResult] {
        override def point[A](aa: => A) = {
          lazy val a = aa
          Success(a, Set(Origin.Point(() => a.toString)))
        }
        override def map[A, B](fa: StepResult[A])(f: A => B) =
          fa map f
        override def ap[A, B](fa: => StepResult[A])(ff: => StepResult[A => B]) =
          (fa, ff) match {
            case (Success(a, o1), Success(f, o2)) => Success(f(a), o1 ++ o2)
            case (f: Failure,     Success(_, _)) => f
            case (Success(_, _),  f: Failure) => f
            case (Failure(x1, x2), Failure(y1, y2)) => Failure(x1 ++ y1, x2 ++ y2)
          }
      }
  }

  sealed abstract class ApiMethod
  object ApiMethod {
    final case class GetOrUse(asString: String) extends ApiMethod
    case object Need extends ApiMethod
    case object Get extends ApiMethod
  }

  def baseGet[A](key: String, apiMethod: ApiMethod, doIt: (Key, Option[Origin.Read]) => StepResult[A]): Config[A] =
    new Config[A] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS { (r, s1) =>
          val k = s1.keyMod(Key(key))

          val (vo: VO[F], s2) =
            s1.queryCache.get(k) match {
              case Some(q) => (q, s1)

              case None =>
                val results: F[Vector[SV]] =
                  r.highToLowPri.toIterator
                    .map { case (name, store) => store(k).map(SV(name, _)) }
                    .toVector
                    .sequenceU
                val selected: F[Option[SV]] =
                  results.map(_.foldLeft[Option[SV]](None) {
                    case (None, nameAndValue) => Some(nameAndValue)
                    case (found@Some(SV(_, _: ConfigValue.Found)), _) => found
                    case (error@Some(SV(_, _: ConfigValue.Error)), _) => error
                    case (Some(SV(_, ConfigValue.NotFound)), next) => Some(next)
                  })
                val q = VO(results, selected)
                (q, s1.copy(queryCache = s1.queryCache.updated(k, q)))
            }

          val s3 = s2.addApi(k, apiMethod)

          val result: F[StepResult[A]] =
            vo.selected.map {
              case None                                     => doIt(k, None)
              case Some(SV(name, found: ConfigValue.Found)) => doIt(k, Some(Origin.Read(k, name, found)))
              case Some(SV(_, ConfigValue.NotFound))        => doIt(k, None)
              case Some(SV(n, e: ConfigValue.Error))        => StepResult.Failure(Map(k -> Some((n, e))), Set.empty)
            }

          ((), result, s3)
        }
    }

  def baseGetA[A, B](key: String, apiMethod: ApiMethod, doIt: (Key, Option[(Origin.Read, A)]) => StepResult[B])(implicit v: ConfigParser[A]): Config[B] =
    baseGet(key, apiMethod, (k, oo) =>
      oo match {
        case Some(o) =>
          v.parse(o.sourceValue) match {
            case \/-(a) => doIt(k, Some((o, a)))
            case -\/(e) => StepResult.Failure(Map(k -> Some((o.sourceName, ConfigValue.Error(e, Some(o.sourceValue.value))))), Set.empty)
          }
        case None => doIt(k, None)
      }
    )

  def keyModUpdate(f: (String => String) => String => String): Config[Unit] =
    new Config[Unit] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F pure ().point[StepResult], s.keyModPush(keyModFS(f(keyModTS(s.keyMod))))))
    }

  def keyModCompose(f: String => String): Config[Unit] =
    keyModUpdate(_ compose f)

  def keyModPop: Config[Unit] =
    new Config[Unit] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F point ().point[StepResult], s.keyModPop))
    }

  def keysUsed: Config[Set[Key]] =
    new Config[Set[Key]] {
      override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F point s.queryCache.keySet.point[StepResult], s))
    }
}
