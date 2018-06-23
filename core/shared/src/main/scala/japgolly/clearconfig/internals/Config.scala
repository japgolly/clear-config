package japgolly.clearconfig.internals

import scalaz._
import Scalaz._
import Evaluation._

/**
  * Representation the desire to read `A` from some as-of-yet-unspecified config.
  */
trait Config[A] extends FailableFunctor[Config, A] {

  def ap[B](f: Config[A => B]): Config[B]

  def run[F[_]](sources: Sources[F])(implicit pp: ValuePreprocessor, F: Monad[F]): F[Result[A]]

  def chooseAttempt[B](f: A => String \/ Config[B]): Config[B]

  /** When a Report is generated, the config values used by this will be obfuscated. */
  def secret: Config[A]

  def withKeyMod(f: String => String): Config[A]

  /**
    * Generate a report based on the usage _so far_.
    * This should be at the very end of your Config composition,
    * else the unused-keys portion of the report may be inaccurate.
    */
  def withReport: Config[(A, Report)]

  /** Use this method very sparingly as it prevents clarity and config discoverability
    * by introducing configuration keys that only appear in certain conditions.
    */
  final def choose[B](f: A => Config[B]): Config[B] = // DO NOT call this flatMap.
    chooseAttempt(a => \/-(f(a)))

  /** Opens up a new bunch of config opens when some other option config value is defined.
    *
    * It's recommended not to use this function in most cases because it hides the optional config from reports when
    * it's not enabled. Where as when a user decides to enable the feature they often want to know what the additional,
    * potentially-mandatory options are. This function makes that an unclear, two-step process.
    */
  final def chooseWhenDefined[B, C](f: B => Config[C])(implicit ev: A =:= Option[B]): Config[Option[C]] =
    choose(ev(_).fold(Option.empty[C].point[Config])(f(_).map(Some(_))))

  //  final def withCaseInsensitiveKeys: Config[A] =
  //    withKeyMod(_.toLowerCase)

  final def withPrefix(prefix: String): Config[A] =
    withKeyMod(prefix + _)
}

object Config {

  def const[A](a: A): Config[A] =
    new Instance[A] {
      override def step[F[_]](implicit F: Monad[F]) =
        Step.ret(a.point[StepResult])
    }

  def get[A: ValueParser](key: String): Config[Option[A]] =
    Instance.baseGetA[A, Option[A]](key, ApiMethod.Get, (_, o) => o match {
      case Some((origin, a)) => StepResult.Success(Some(a), Set(origin))
      case None              => StepResult.Success(None, Set.empty)
    })

  def getOrUse[A: ValueParser](key: String, default: A): Config[A] =
    Instance.baseGetA[A, A](key, ApiMethod.GetOrUse(default.toString), (_, o) => o match {
      case Some((origin, a)) => StepResult.Success(a, Set(origin))
      case None              => StepResult.Success(default, Set.empty)
    })

  def need[A: ValueParser](key: String): Config[A] =
    Instance.baseGetA[A, A](key, ApiMethod.Need, (k, o) => o match {
      case Some((origin, a)) => StepResult.Success(a, Set(origin))
      case None              => StepResult.Failure(Map(k -> None), Set.empty)
    })

  def consumerFn[B] = new ConsumerFn[B]
  final class ConsumerFn[B] extends {
    def get[A](k: String, f: B => A => Unit)(implicit r: ValueParser[A]): Config[B => Unit] =
      Config.get(k)(r).map(oa => b => oa.fold(())(f(b)))

    def getOrUse[A](k: String, f: B => A => Unit)(default: => A)(implicit r: ValueParser[A]): Config[B => Unit] =
      Config.get(k)(r).map(oa => f(_)(oa getOrElse default))

    def need[A](k: String, f: B => A => Unit)(implicit r: ValueParser[A]): Config[B => Unit] =
      Config.need(k)(r).map(a => f(_)(a))

    def getC[A](k: String, f: (B, A) => Unit)(implicit r: ValueParser[A]): Config[B => Unit] =
      get[A](k, f.curried)

    def getOrUseC[A](k: String, f: (B, A) => Unit)(default: => A)(implicit r: ValueParser[A]): Config[B => Unit] =
      getOrUse[A](k, f.curried)(default)

    def needC[A](k: String, f: (B, A) => Unit)(implicit r: ValueParser[A]): Config[B => Unit] =
      need[A](k, f.curried)

    def apply(cs: (ConsumerFn[B] => Config[B => Unit])*): Config[B => Unit] =
      mergeConsumerFns(cs.map(_ apply this): _*)
  }


  def mergeConsumerFns[A](cs: Config[A => Unit]*): Config[A => Unit] =
    if (cs.isEmpty)
      ((_: A) => ()).pure[Config]
    else
      cs.reduce(applicativeInstance.apply2(_, _)((f, g) => a => { f(a); g(a) }))

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  
  implicit val applicativeInstance: Applicative[Config] =
    new Applicative[Config] {
      override def point[A](a: => A)                                 = Config.const(a)
      override def map[A, B](fa: Config[A])(f: A => B)               = fa map f
      override def ap[A, B](fa: => Config[A])(ff: => Config[A => B]) = fa.ap(ff)
    }
  
  private implicit def configToInstance[A](c: Config[A]): Instance[A] =
    c match {
      case i: Instance[A] => i
    }

  private trait Instance[A] extends Config[A] { self =>
    def step[F[_]](implicit F: Monad[F]): Step[F, StepResult[A]]

    override final def ap[B](ff: Config[A => B]): Config[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]) = {
          // type X[Y] = F[(R[F], S[F]) => F[(Unit, StepResult[Y], S[F])]]
          val xa = self.step(F).getF[S[F], R[F]](F) // : X[A]
          val xf = ff.step(F).getF[S[F], R[F]](F) // : X[A => B]
          Step[F, StepResult[B]] { (r, s0) =>
            for {
              gf          ← xf
              ga          ← xa
              hf          ← gf(r, s0)
              (_, ff, s1) = hf
              ha          ← ga(r, s1)
              (_, fa, s2) = ha
            } yield (s2, StepResult.scalazInstance.ap(fa)(ff))
          }
        }
      }

    override final def run[F[_]](sources: Sources[F])(implicit pp: ValuePreprocessor, F: Monad[F]): F[Result[A]] = {
      type OK = (SourceName, Store[F])
      type KO = (SourceName, String)

      // Prepare sources
      val r1: Vector[F[KO \/ OK]] = sources.highToLowPri.map(s => F.map(s.prepare)(_.bimap(s.name -> _, s.name -> _.mapValues(pp.run))))
      val r2: F[Vector[KO \/ OK]] = r1.sequence
      val r3: F[KO \/ Vector[OK]] = F.map(r2)(_.sequenceU)

      F.bind(r3) {
        case \/-(stores) =>

          // Read config
          step(F).run(R(stores), S.init).map { case (_, stepResult, _) =>
            stepResult match {
              case StepResult.Success(a, _) => Result.Success(a)
              case StepResult.Failure(x, y) => Result.QueryFailure(x, y.map(_.public), sources.highToLowPri.map(_.name))
            }
          }

        case -\/((s, err)) => F.pure(Result.PreparationFailure(s, err))
      }
    }

    override final def map[B](f: A => B): Config[B] =
      stepMap(ra => ra.flatMap(a => StepResult.Success(f(a), Set(Origin.Map))))

    override final def mapAttempt[B](f: A => String \/ B): Config[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]) =
          self.step(F) map {
            case StepResult.Success(a, originsA) =>
              f(a) match {
                case \/-(b) => StepResult.Success(b, originsA)
                case -\/(e) => StepResult.fail(e, originsA)
              }
            case f: StepResult.Failure => f
          }
      }

    final def stepMap[B](f: StepResult[A] => StepResult[B]): Config[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]) =
          self.step(F).map(f)
      }

    override final def chooseAttempt[B](f: A => String \/ Config[B]): Config[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]): Step[F, StepResult[B]] =
          self.step(F).flatMap {
            case StepResult.Success(a, originsA) =>
              f(a) match {
                case \/-(cb) => cb.step(F)
                case -\/(e)  => Step.ret(StepResult.fail(e, originsA))
              }
            case f: StepResult.Failure => Step.ret(f)
          }
      }

    override final def secret: Config[A] =
      new Instance[A] {
        def step[F[_]](implicit F: Monad[F]) = {
          val x = Step.monad[F]
          def keys(s1: S[F], s2: S[F]): Vector[Key] = s2.queryLog.drop(s1.queryLog.length)
          for {
            s <- x.get
            a <- self.step(F)
            _ <- x.modify(s2 => s2.obfuscateKeys(keys(s, s2)))
          } yield a
        }
      }

    override final def withKeyMod(f: String => String): Config[A] =
      Instance.keyModCompose(f) *> this <* Instance.keyModPop

    override final def withReport: Config[(A, Report)] =
      (this: Config[A]) tuple Config.Instance.reportSoFar
  }

  private object Instance {
    private def keyModTS(f: Key => Key): String => String = s => f(Key(s)).value
    private def keyModFS(f: String => String): Key => Key = k => Key(f(k.value))

    def baseGet[A](key: String, apiMethod: ApiMethod, doIt: (Key, Option[Origin.Read]) => StepResult[A]): Config[A] =
      new Instance[A] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step { (r, s1) =>
            val k = s1.keyMod(Key(key))

            val (vo: QueryResult[F], s2) =
              s1.queryCache.get(k) match {
                case Some(q) => (q, s1)

                case None =>
                  val results: F[Vector[SrcAndVal]] =
                    r.highToLowPri.toIterator
                      .map { case (name, store) => store(k).map(SrcAndVal(name, _)) }
                      .toVector
                      .sequence
                  val selected: F[Option[SrcAndVal]] =
                    results.map(_.foldLeft[Option[SrcAndVal]](None) {
                      case (None, nameAndValue) => Some(nameAndValue)
                      case (found@Some(SrcAndVal(_, _: Lookup.Found)), _) => found
                      case (error@Some(SrcAndVal(_, _: Lookup.Error)), _) => error
                      case (Some(SrcAndVal(_, Lookup.NotFound)), next) => Some(next)
                    })
                  val q = QueryResult(results, selected)
                  (q, s1.copy(queryCache = s1.queryCache.updated(k, q)))
              }

            val s3 = s2.addApi(k, apiMethod).logQuery(k)

            val result: F[StepResult[A]] =
              vo.selected.map {
                case None                                            => doIt(k, None)
                case Some(SrcAndVal(name, found: Lookup.Found)) => doIt(k, Some(Origin.Read(k, name, found)))
                case Some(SrcAndVal(_, Lookup.NotFound))        => doIt(k, None)
                case Some(SrcAndVal(n, e: Lookup.Error))        => StepResult.Failure(Map(k -> Some((n, e))), Set.empty)
              }

            // println(s"After [$k] - $s3")
            result.map((s3, _))
          }
      }

    def baseGetA[A, B](key: String, apiMethod: ApiMethod, doIt: (Key, Option[(Origin.Read, A)]) => StepResult[B])(implicit v: ValueParser[A]): Config[B] =
      baseGet(key, apiMethod, (k, oo) =>
        oo match {
          case Some(o) =>
            v.parse(o.sourceValue) match {
              case \/-(a) => doIt(k, Some((o, a)))
              case -\/(e) => StepResult.Failure(Map(k -> Some((o.sourceName, Lookup.Error(e, Some(o.sourceValue.value))))), Set.empty)
            }
          case None => doIt(k, None)
        }
      )

    def keyModUpdate(f: (String => String) => String => String): Config[Unit] =
      new Instance[Unit] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step((_, s) => (s.keyModPush(keyModFS(f(keyModTS(s.keyMod)))), ().point[StepResult]).point[F])
      }

    def keyModCompose(f: String => String): Config[Unit] =
      keyModUpdate(_ compose f)

    def keyModPop: Config[Unit] =
      new Instance[Unit] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step((_, s) => (s.keyModPop, ().point[StepResult]).point[F])
      }

    def keysUsed: Config[Set[Key]] =
      new Instance[Set[Key]] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step((_, s) => (s, s.queryCache.keySet.point[StepResult]).point[F])
      }

    def reportSoFar: Config[Report] =
      new Instance[Report] {
        def step[F[_]](implicit F: Monad[F]) =
          Step((r, s) => ReportCreation(r, s).map(a => (s, a.point[StepResult])))
      }

  }
}
