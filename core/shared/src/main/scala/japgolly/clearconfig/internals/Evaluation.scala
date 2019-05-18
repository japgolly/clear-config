package japgolly.clearconfig.internals

import scalaz._
import Scalaz._
import japgolly.microlibs.stdlib_ext.StdlibExt._

private[internals] object Evaluation {

  type Step[F[_], A] = RWST[F, R[F], Unit, S[F], A]
  object Step {
    def apply[F[_], A](f: (R[F], S[F]) => F[(S[F], A)])(implicit F: Functor[F]): Step[F, A] =
      RWST((r, s) => F.map(f(r, s))(x => ((), x._2, x._1)))

    def monad[F[_]: Monad] =
      ReaderWriterStateT.rwstMonad[F, R[F], Unit, S[F]]

    def ret[F[_], A](a: A)(implicit F: Applicative[F]): Step[F, A] =
      retF(F.point(a))

    def retF[F[_], A](fa: F[A])(implicit F: Applicative[F]): Step[F, A] =
      RWST((_, s) => fa.map(((), _, s)))
  }

  final case class R[F[_]](highToLowPri: Vector[(SourceName, Store[F])])

  /** State */
  final case class S[F[_]](keyModStack    : List[Key => Key],
                           queryCache     : Map[Key, QueryResult[F]],
                           apiData        : Map[Key, Set[ApiMethod]],
                           queryLog       : Vector[Key],
                           keysToObfuscate: Set[Key]) {

    def keyMod: Key => Key =
      keyModStack.headOption.getOrElse(identity[Key])

    def keyModPush(f: Key => Key): S[F] =
      copy(f :: keyModStack)

    def keyModPop: S[F] =
      keyModStack match {
        case Nil => this
        case _ :: t => copy(t)
      }

    def logQuery(key: Key): S[F] =
      copy(queryLog = queryLog :+ key)

    def addApi(key: Key, apiMethod: ApiMethod): S[F] =
      copy(apiData = apiData.initAndModifyValue(key, Set.empty, _ + apiMethod))

    def obfuscateKeys(keys: TraversableOnce[Key]): S[F] =
      copy(keysToObfuscate = keysToObfuscate ++ keys)

    def queriesSince(previous: S[F]) =
      queryLog.drop(previous.queryLog.length)
  }

  object S {
    def init[F[_]]: S[F] =
      S(Nil, Map.empty, Map.empty, Vector.empty, Set.empty)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class SrcAndVal(source: SourceName, value: Lookup)
  final case class QueryResult[F[_]](highToLowPri: F[Vector[SrcAndVal]], selected: F[Option[SrcAndVal]])

  sealed abstract class Origin
  object Origin {
    final case class Point(value: () => String) extends Origin
    final case class Read(key: Key, sourceName: SourceName, sourceValue: Lookup.Found) extends Origin
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

    final case class Failure(keyed: Map[Key, Option[(SourceName, Lookup.Error)]],
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

    implicit val scalazInstance: Monad[StepResult] =
      new Monad[StepResult] {
        override def point[A](aa: => A) = {
          lazy val a = aa
          Success(a, Set(Origin.Point(() => "" + a)))
        }
        override def ap[A, B](fa: => StepResult[A])(ff: => StepResult[A => B]) =
          (fa, ff) match {
            case (Success(a, o1) , Success(ab, o2)) => Success(ab(a), o1 ++ o2)
            case (f: Failure     , Success(_, _)  ) => f
            case (Success(_, _)  , f: Failure     ) => f
            case (Failure(x1, x2), Failure(y1, y2)) => Failure(x1 ++ y1, x2 ++ y2)
          }
        override def map[A, B](fa: StepResult[A])(f: A => B) =
          fa map f
        override def bind[A, B](fa: StepResult[A])(f: A => StepResult[B]) =
          fa flatMap f
      }

    def fail(errorMsg: String, origins: Set[Origin]): Failure = {
      var keyed = Map.empty[Key, Option[(SourceName, Lookup.Error)]]
      var other = Set.empty[UnkeyedError]
      origins.iterator.take(2).toList match {
        case (o: Origin.Read) :: Nil =>
          val err = Lookup.Error(errorMsg, Some(o.sourceValue.value))
          keyed += o.key -> Some((o.sourceName, err))
        case _ =>
          other += UnkeyedError(errorMsg, origins)
      }
      Failure(keyed, other)
    }
  }

  sealed abstract class ApiMethod
  object ApiMethod {
    final case class GetOrUse(asString: String) extends ApiMethod
    case object Need extends ApiMethod
    case object Get extends ApiMethod
  }
}
