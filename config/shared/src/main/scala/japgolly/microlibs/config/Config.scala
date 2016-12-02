package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext._, StdlibExt._
import java.util.Properties
import java.util.regex.Pattern
/*
import scalaz.{-\/, \/, \/-}
import scalaz.{Applicative, Apply, Bind, Monad, RWS}
import scalaz.Scalaz.Id
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._
import scalaz.std.vector.vectorInstance
*/
import scalaz._, Scalaz._

case class Key(value: String) extends AnyVal

// ===================================================================================================================

/**
  * Representation the desire to read `A` from some as-of-yet-unspecified config.
  */
trait Config[A] {
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
        case StepResult.Success(a) => ConfigResult.Success(a)
        case StepResult.Failure(m) => ConfigResult.QueryFailure(m)
      }
      case -\/((s, err)) => F.pure(ConfigResult.PreparationFailure(s, err))
    }
  }

  final def withReport: Config[(A, Report)] =
    this tuple Config.keyReport

  final def map[B](f: A => B): Config[B] =
    mapAttempt(a => StepResult.Success(f(a)))

  final def mapAttempt[B](f: A => StepResult[B]): Config[B] = {
    val self = this
    new Config[B] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        self.step(F).map(F.map(_)(_ flatMap f))
    }
  }

  final def withKeyMod(f: String => String): Config[A] =
    Config.keyModCompose(f) *> this <* Config.keyModPop

  final def withCaseInsensitiveKeys: Config[A] =
    withKeyMod(_.toLowerCase)

  final def withPrefix(prefix: String): Config[A] =
    withKeyMod(prefix + _)
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
  }

  private[config] object StepResult {

    final case class Failure(failures: Map[Key, Option[(SourceName, ConfigValue.Error)]]) extends StepResult[Nothing] {
      override def map[B](f: Nothing => B): StepResult[B] = this
      override def flatMap[B](f: Nothing => StepResult[B]): StepResult[B] = this
    }

    final case class Success[+A](value: A) extends StepResult[A] {
      override def map[B](f: A => B): StepResult[B] = Success(f(value))
      override def flatMap[B](f: A => StepResult[B]): StepResult[B] = f(value)
    }

    implicit val applicativeInstance: Applicative[StepResult] =
      new Applicative[StepResult] {
        override def point[A](a: => A) = Success(a)
        override def map[A, B](fa: StepResult[A])(f: A => B) = fa map f
        override def ap[A, B](fa: => StepResult[A])(ff: => StepResult[A => B]) =
          (fa, ff) match {
            case (Success(a), Success(f)) => Success(f(a))
            case (f: Failure, Success(_)) => f
            case (Success(_), f: Failure) => f
            case (Failure(x), Failure(y)) => Failure(x ++ y)
          }
      }
  }

  implicit val applicativeInstance: Applicative[Config] =
    new Applicative[Config] {
      override def point[A](a: => A) = new Config[A] {
        private[config] override def step[F[_]](implicit F: Applicative[F]) =
          RWS((r, s) => ((), F.point(StepResult.Success(a)), s))
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

  private def baseGet[A, B](key: String, doIt: (Key, Option[A]) => StepResult[B])(implicit v: ValueReader[A]): Config[B] =
    new Config[B] {
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

          val result: F[StepResult[B]] =
            vo.selected.map {
              case None => doIt(k, None)
              case Some(SV(name, found: ConfigValue.Found)) =>
                v.read(found) match {
                  case \/-(a) => doIt(k, Some(a))
                  case -\/(e) => StepResult.Failure(Map(k -> Some((name, ConfigValue.Error(e, Some(found.value))))))
                }
              case Some(SV(_, ConfigValue.NotFound)) => doIt(k, None)
              case Some(SV(n, e: ConfigValue.Error)) => StepResult.Failure(Map(k -> Some((n, e))))
            }

          ((), result, s2)
        }
    }

  def get[A: ValueReader](key: String): Config[Option[A]] =
    baseGet[A, Option[A]](key, (_, oa) => StepResult.Success(oa))

  def get[A: ValueReader](key: String, default: => A): Config[A] =
    get[A](key).map(_ getOrElse default)

  def need[A: ValueReader](key: String): Config[A] =
    baseGet[A, A](key, (k, oa) => oa match {
      case Some(a) => StepResult.Success(a)
      case None    => StepResult.Failure(Map(k -> None))
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
              StepResult.Success(
                Report.withDefaults(r.highToLowPri.map(_._1), used, unused)))

          ((), result, s)
        }
    }

  private[config] def keyModUpdate(f: (String => String) => String => String): Config[Unit] =
    new Config[Unit] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F pure StepResult.Success(()), s.keyModPush(keyModFS(f(keyModTS(s.keyMod))))))
    }

  private[config] def keyModCompose(f: String => String): Config[Unit] =
    keyModUpdate(_ compose f)

  private[config] def keyModPop: Config[Unit] =
    new Config[Unit] {
      private[config] override def step[F[_]](implicit F: Applicative[F]) =
        RWS((_, s) => ((), F point StepResult.Success(()), s.keyModPop))
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

  final case class QueryFailure(failures: Map[Key, Option[(SourceName, ConfigValue.Error)]]) extends ConfigResult[Nothing] {
    def errorMsg: String = {
      val each = failures.toVector.sortBy(_._1.value.toLowerCase).map {
        case (Key(k), None) =>
          s"No value for key [$k]"
        case (Key(k), Some((SourceName(src), ConfigValue.Error(desc, None)))) =>
          s"Error reading key [$k] from source [$src]: $desc"
        case (Key(k), Some((SourceName(src), ConfigValue.Error(desc, Some(v))))) =>
          s"Error reading key [$k] from source [$src] with value [$v]: $desc"
      }
      var errors = "error"
      if (each.length != 1) errors += "s"
      s"${each.length} $errors:${each.map("\n  - " + _).sorted.mkString}"
    }
    override def toDisjunction = -\/(errorMsg)
  }

  final case class Success[+A](value: A) extends ConfigResult[A] {
    override def toDisjunction = \/-(value)
  }
}
