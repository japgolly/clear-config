package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext._, StdlibExt._
import scalaz._, Scalaz._

case class Key(value: String) extends AnyVal

// ===================================================================================================================

trait Ass[F[_], A] {
  def mapAttempt[B](f: A => String \/ B): F[B]

  def test(errorMsg: A => Option[String]): F[A] =
    mapAttempt(a => errorMsg(a).fold[String \/ A](\/-(a))(-\/.apply))

  def ensure(test: A => Boolean, errorMsg: => String): F[A] =
    this.test(a => if (test(a)) None else Some(errorMsg))

  def mapCatch[B](f: A => B, e: Throwable => String = _.toString): F[B] =
    mapAttempt(a => \/.fromTryCatchNonFatal(f(a)).leftMap(e))

  def mapOption[B](f: A => Option[B], errorMsg: => String = "Not a recognised value."): F[B] =
    mapAttempt(f(_).fold[String \/ B](-\/(errorMsg))(\/-.apply))
}

/**
  * Representation the desire to read `A` from some as-of-yet-unspecified config.
  */
sealed trait Config[A] extends Ass[Config, A] {
  import Config.{R, S, Step, StepResult}

  private[config] def step[F[_]](implicit F: Applicative[F]): Step[F, StepResult[A]]

  final def run[F[_]](sources: Sources[F])(implicit F: Monad[F]): F[ConfigResult[A]] = {
    type OK = (SourceName, ConfigStore[F])
    type KO = (SourceName, String)

    val r1: Vector[F[KO \/ OK]] = sources.highToLowPri.map(s => F.map(s.prepare)(_.bimap(s.name -> _, s.name -> _)))
    val r2: F[Vector[KO \/ OK]] = r1.sequenceU
    val r3: F[KO \/ Vector[OK]] = F.map(r2)(_.sequenceU)

    F.bind(r3) {
      case \/-(stores) => step(F).run(R(stores), S.init)._2.map {
        case StepResult.Success(a, _) => ConfigResult.Success(a)
        case StepResult.Failure(x, y) => ConfigResult.QueryFailure(x, y.map(_.public))
      }
      case -\/((s, err)) => F.pure(ConfigResult.PreparationFailure(s, err))
    }
  }

  final def map[B](f: A => B): Config[B] =
    stepMap(ra => ra.flatMap(a => StepResult.Success(f(a), Set(StepResult.Origin.Map))))

  override final def mapAttempt[B](f: A => String \/ B): Config[B] = {
    val self = this
    new Config[B] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        for {
          ra <- self.step(F)
          ks <- Config.keysUsed.step(F)
        } yield
          ra.map {
            case StepResult.Success(a, os) =>
              f(a) match {
                case \/-(b) => StepResult.Success(b, os)
                case -\/(e) =>

                  var keyed = Map.empty[Key, Option[(SourceName, ConfigValue.Error)]]
                  var other = Set.empty[StepResult.XXX]
                  os.iterator.take(2).toList match {
                    case (o: StepResult.Origin.Read) :: Nil =>
                      val err = ConfigValue.Error(e, Some(o.sourceValue.value))
                      keyed += o.key -> Some ((o.sourceName, err))
                    case _ =>
                      other += StepResult.XXX(e, os)
                  }
                  StepResult.Failure(keyed, other)
              }

            case f: StepResult.Failure => f
          }
    }
  }

  private[config] final def stepMap[B](f: StepResult[A] => StepResult[B]): Config[B] = {
    val self = this
    new Config[B] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        self.step(F).map(F.map(_)(f))
    }
  }

  final def withKeyMod(f: String => String): Config[A] =
    Config.keyModCompose(f) *> this <* Config.keyModPop

  final def withCaseInsensitiveKeys: Config[A] =
    withKeyMod(_.toLowerCase)

  final def withPrefix(prefix: String): Config[A] =
    withKeyMod(prefix + _)

  final def withReport: Config[(A, Report)] =
    this tuple Config.keyReport
}

object Config {
  private val IdMonad = implicitly[Monad[Id]]

  private[config] type Step[F[_], A] = RWS[R[F], Unit, S[F], F[A]]

  private[config] final case class R[F[_]](highToLowPri: Vector[(SourceName, ConfigStore[F])])

  private[config] final case class S[F[_]](keyModStack: List[Key => Key], queryCache: Map[Key, VO[F]]) {
    def keyMod: Key => Key =
      keyModStack.headOption.getOrElse(identity[Key])
    def keyModPush(f: Key => Key): S[F] =
      copy(f :: keyModStack)
    def keyModPop: S[F] =
      keyModStack match {
        case Nil => this
        case _ :: t => copy(t)
      }
  }
  private[config] object S {
    def init[F[_]]: S[F] = S(Nil, Map.empty)
  }

  private[config] case class SV(source: SourceName, value: ConfigValue)
  private[config] case class VO[F[_]](highToLowPri: F[Vector[SV]], selected: F[Option[SV]])

  private[config] sealed abstract class StepResult[+A] {
    def map[B](f: A => B): StepResult[B]
    def flatMap[B](f: A => StepResult[B]): StepResult[B]
    def addOrigins(o: Set[StepResult.Origin]): StepResult[A]
  }

  private[config] object StepResult {

    sealed abstract class Origin
    object Origin {
      final case class Point(value: () => String) extends Origin
      final case object Map extends Origin
      final case class Read(key: Key, sourceName: SourceName, sourceValue: ConfigValue.Found) extends Origin
    }

    final case class XXX(error: String, origins: Set[Origin]) {
      def public: (String, Set[String \/ Key]) =
        (error, origins.map {
          case Origin.Map => -\/("<function>")
          case Origin.Point(f) =>
            -\/(f() match {
              case s if s.matches("^<function\\d+>$") => "<function>"
              case s => s"runtime value [$s]"
            })
          case Origin.Read(k, _, _) => \/-(k)
        })
    }

    final case class Failure(keyed: Map[Key, Option[(SourceName, ConfigValue.Error)]],
                             other: Set[XXX]) extends StepResult[Nothing] {
      override def map[B](f: Nothing => B): StepResult[B] = this
      override def flatMap[B](f: Nothing => StepResult[B]): StepResult[B] = this
      override def addOrigins(o: Set[StepResult.Origin]) = this
    }

    final case class Success[+A](value: A, origins: Set[Origin]) extends StepResult[A] {
      override def map[B](f: A => B): StepResult[B] = Success(f(value), origins)
      override def flatMap[B](f: A => StepResult[B]): StepResult[B] = f(value) addOrigins origins
      override def addOrigins(o: Set[StepResult.Origin]) = Success(value, origins ++ o)
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

  implicit val applicativeInstance: Applicative[Config] =
    new Applicative[Config] {
      override def point[A](a: => A) = new Config[A] {
        private[config] override def step[F[_]](implicit F: Applicative[F]) =
          RWS((r, s) => ((), F.point(StepResult.applicativeInstance point a), s))
      }
      override def map[A, B](fa: Config[A])(f: A => B) =
        fa map f
      override def ap[A, B](fa: => Config[A])(ff: => Config[A => B]) = new Config[B] {
        private[config] override def step[F[_]](implicit F: Applicative[F]) = {
          val ga = fa.step[F].getF[S[F], R[F]](IdMonad)
          val gf = ff.step[F].getF[S[F], R[F]](IdMonad)
          val FR = F.compose(StepResult.applicativeInstance)
          RWS { (r, s0) =>
            val (_, ff, s1) = gf(r, s0)
            val (_, fa, s2) = ga(r, s1)
            ((), FR.ap(fa)(ff), s2)
          }
        }
      }
    }

  private def baseGet[A](key: String, doIt: (Key, Option[StepResult.Origin.Read]) => StepResult[A]): Config[A] =
    new Config[A] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
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

          val result: F[StepResult[A]] =
            vo.selected.map {
              case None                                     => doIt(k, None)
              case Some(SV(name, found: ConfigValue.Found)) => doIt(k, Some(StepResult.Origin.Read(k, name, found)))
              case Some(SV(_, ConfigValue.NotFound))        => doIt(k, None)
              case Some(SV(n, e: ConfigValue.Error))        => StepResult.Failure(Map(k -> Some((n, e))), Set.empty)
            }

          ((), result, s2)
        }
    }

  private def baseGetA[A, B](key: String, doIt: (Key, Option[(StepResult.Origin.Read, A)]) => StepResult[B])(implicit v: ConfigParser[A]): Config[B] =
    baseGet(key, (k, oo) =>
      oo match {
        case Some(o) =>
          v.parse(o.sourceValue) match {
            case \/-(a) => doIt(k, Some((o, a)))
            case -\/(e) => StepResult.Failure(Map(k -> Some((o.sourceName, ConfigValue.Error(e, Some(o.sourceValue.value))))), Set.empty)
          }
        case None => doIt(k, None)
      }
    )

  def get[A: ConfigParser](key: String): Config[Option[A]] =
    baseGetA[A, Option[A]](key, (_, o) => o match {
      case Some((origin, a)) => StepResult.Success(Some(a), Set(origin))
      case None              => StepResult.Success(None, Set.empty)
    })

  def get[A: ConfigParser](key: String, default: => A): Config[A] =
    get[A](key).map(_ getOrElse default)

  def need[A: ConfigParser](key: String): Config[A] =
    baseGetA[A, A](key, (k, o) => o match {
      case Some((origin, a)) => StepResult.Success(a, Set(origin))
      case None              => StepResult.Failure(Map(k -> None), Set.empty)
    })

  def keyReport: Config[Report] =
    new Config[Report] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        RWS { (r, s) =>

          type M = Map[Key, Map[SourceName, ConfigValue]]
          def emptyM: M = Map.empty
          implicit def semigroupConfigValue: Semigroup[ConfigValue] =
            Semigroup.firstSemigroup // There will never be overlap

          val fUsed: F[M] =
            s.queryCache
              .toVector
              .traverse { case (k, x) => x.highToLowPri.map(x => k -> x.toIterator.map(sv => sv.source -> sv.value).toMap) }
              .map(_.foldLeft(emptyM) { case (m, (k, vs)) => m.modifyValue(k, _.fold(vs)(_ ++ vs)) })

          val usedKeys = s.queryCache.keySet

          val fUnused: F[M] =
            r.highToLowPri.traverse { case (src, store) =>
              store.getBulk(!usedKeys.contains(_))
                .map(_.mapValuesNow(value => Map(src -> ConfigValue.Found(value))))
            }.map(_.foldLeft(emptyM)(_ |+| _))


          val result: F[StepResult[Report]] =
            F.apply2(fUsed, fUnused)((used, unused) =>
              Report.withDefaults(r.highToLowPri.map(_._1), used, unused).point[StepResult])

          ((), result, s)
        }
    }

  private[config] def keyModUpdate(f: (String => String) => String => String): Config[Unit] =
    new Config[Unit] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F pure ().point[StepResult], s.keyModPush(keyModFS(f(keyModTS(s.keyMod))))))
    }

  private[config] def keyModCompose(f: String => String): Config[Unit] =
    keyModUpdate(_ compose f)

  private[config] def keyModPop: Config[Unit] =
    new Config[Unit] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F point ().point[StepResult], s.keyModPop))
    }

  private[config] def keysUsed: Config[Set[Key]] =
    new Config[Set[Key]] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F point s.queryCache.keySet.point[StepResult], s))
    }
}

sealed abstract class ConfigResult[+A] {
  def toDisjunction: String \/ A
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
