package japgolly.clearconfig.internals

import cats._
import cats.data.ReaderWriterStateT
import cats.instances.all._
import cats.syntax.all._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.annotation.tailrec
import scala.collection.compat._

private[internals] object Evaluation {

  type Step[F[_], A] = ReaderWriterStateT[F, R[F], Unit, S[F], A]

  object Step {
    def apply[F[_], A](f: (R[F], S[F]) => F[(S[F], A)])(implicit F: Applicative[F]): Step[F, A] =
      ReaderWriterStateT((r, s) => F.map(f(r, s))(x => ((), x._1, x._2)))

    def monad[F[_]: Monad] =
      Monad[ReaderWriterStateT[F, R[F], Unit, S[F], *]]

    def get[F[_]: Applicative] =
      ReaderWriterStateT.get[F, R[F], Unit, S[F]]

    def modify[F[_]: Applicative](f: S[F] => S[F]) =
      ReaderWriterStateT.modify[F, R[F], Unit, S[F]](f)

    def ret[F[_], A](a: A)(implicit F: Applicative[F]): Step[F, A] =
      retF(F.pure(a))

    def retF[F[_], A](fa: F[A])(implicit F: Applicative[F]): Step[F, A] =
      ReaderWriterStateT((_, s) => fa.map(((), s, _)))
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

    def obfuscateKeys(keys: IterableOnce[Key]): S[F] =
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
    def public: (String, Set[Either[String, Key]]) =
      (error, origins.map {
        case Origin.Map => Left("<function>")
        case Origin.Point(f) =>
          Left(f() match {
            case s if s.matches("^<function\\d+>$") | s.contains("$Lambda$") => "<function>"
            case s => s"runtime value [$s]"
          })
        case Origin.Read(k, _, _) => Right(k)
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

    implicit val catsInstance: Monad[StepResult] =
      new Monad[StepResult] {

        override def tailRecM[A, B](z: A)(f: A => StepResult[Either[A,B]]): StepResult[B] = {
          @tailrec
          def go(a: A, origins: Set[Origin]): StepResult[B] =
            f(a) match {
              case Success(Left(a), o)  => go(a, origins ++ o)
              case Success(Right(b), o) => Success(b, origins ++ o)
              case f: Failure           => f
            }
          go(z, Set.empty)
        }

        override def pure[A](a: A) =
          Success(a, Set(Origin.Point(() => "" + a)))

        override def ap[A, B](ff: StepResult[A => B])(fa: StepResult[A]) =
          (fa, ff) match {
            case (Success(a, o1) , Success(ab, o2)) => Success(ab(a), o1 ++ o2)
            case (f: Failure     , Success(_, _)  ) => f
            case (Success(_, _)  , f: Failure     ) => f
            case (Failure(x1, x2), Failure(y1, y2)) => Failure(x1 ++ y1, x2 ++ y2)
          }

        override def map[A, B](fa: StepResult[A])(f: A => B) =
          fa map f

        override def flatMap[A, B](fa: StepResult[A])(f: A => StepResult[B]) =
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
