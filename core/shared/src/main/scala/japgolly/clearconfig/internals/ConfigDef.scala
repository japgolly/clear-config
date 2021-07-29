package japgolly.clearconfig.internals

import cats._
import cats.instances.all._
import cats.syntax.all._
import japgolly.clearconfig.internals.Evaluation._

/**
  * Representation the desire to read `A` from some as-of-yet-unspecified config.
  */
trait ConfigDef[A] extends FailableFunctor[ConfigDef, A] {

  def ap[B](f: ConfigDef[A => B]): ConfigDef[B]

  def run[F[_]](sources: Sources[F])(implicit pp: ValuePreprocessor, F: Monad[F]): F[Result[A]]

  def chooseAttempt[B](f: A => Either[String, ConfigDef[B]]): ConfigDef[B]

  /** Requires at least one key to be externally specified, else `None` is returned.
    *
    * If some keys are available and some required others are not, then it is a failure.
    */
  def option: ConfigDef[Option[A]]

  /** When a Report is generated, the config values used by this will be obfuscated. */
  def secret: ConfigDef[A]

  def withKeyMod(f: String => String): ConfigDef[A]

  /**
    * Generate a report based on the usage _so far_.
    * This should be at the very end of your Config composition,
    * else the unused-keys portion of the report may be inaccurate.
    */
  def withReport(implicit s: Report.Settings): ConfigDef[(A, Report)]

  /** Use this method very sparingly as it prevents clarity and config discoverability
    * by introducing configuration keys that only appear in certain conditions.
    */
  final def choose[B](f: A => ConfigDef[B]): ConfigDef[B] = // DO NOT call this flatMap.
    chooseAttempt(a => Right(f(a)))

  /** Opens up a new bunch of config opens when some other option config value is defined.
    *
    * It's recommended not to use this function in most cases because it hides the optional config from reports when
    * it's not enabled. Where as when a user decides to enable the feature they often want to know what the additional,
    * potentially-mandatory options are. This function makes that an unclear, two-step process.
    */
  final def chooseWhenDefined[B, C](f: B => ConfigDef[C])(implicit ev: A =:= Option[B]): ConfigDef[Option[C]] =
    choose(ev(_).fold(Option.empty[C].pure[ConfigDef])(f(_).map(Some(_))))

  final def withPrefix(prefix: String): ConfigDef[A] =
    withKeyMod(prefix + _)
}

object ConfigDef {

  def unit: ConfigDef[Unit] =
    const(())

  def const[A](a: A): ConfigDef[A] =
    new Instance[A] {
      override def step[F[_]](implicit F: Monad[F]) =
        Step.ret(a.pure[StepResult])
    }

  def get[A: ValueParser](key: String): ConfigDef[Option[A]] =
    Instance.baseGetA[A, Option[A]](key, ApiMethod.Get, (_, o) => o match {
      case Some((origin, a)) => StepResult.Success(Some(a), Set(origin))
      case None              => StepResult.Success(None, Set.empty)
    })

  def getOrUse[A: ValueParser](key: String, default: A): ConfigDef[A] =
    Instance.baseGetA[A, A](key, ApiMethod.GetOrUse("" + default), (_, o) => o match {
      case Some((origin, a)) => StepResult.Success(a, Set(origin))
      case None              => StepResult.Success(default, Set.empty)
    })

  def need[A: ValueParser](key: String): ConfigDef[A] =
    Instance.baseGetA[A, A](key, ApiMethod.Need, (k, o) => o match {
      case Some((origin, a)) => StepResult.Success(a, Set(origin))
      case None              => StepResult.Failure(Map(k -> None), Set.empty)
    })

  /** Mark keys as being used externally so that they appear in the used portion of the ConfigReport,
    * instead of unused.
    */
  def external(keys: String*): ConfigDef[Unit] =
    keys.foldLeft(unit)(_ <* get[String](_)(ValueParser.id))

  def consumerFn[B] = new ConsumerFn[B]
  final class ConsumerFn[B] {
    def get[A](k: String, f: B => A => Unit)(implicit r: ValueParser[A]): ConfigDef[B => Unit] =
      ConfigDef.get(k)(r).map(oa => b => oa.fold(())(f(b)))

    def getOrUse[A](k: String, f: B => A => Unit)(default: A)(implicit r: ValueParser[A]): ConfigDef[B => Unit] =
      ConfigDef.getOrUse(k, default).map(a => f(_)(a))

    def need[A](k: String, f: B => A => Unit)(implicit r: ValueParser[A]): ConfigDef[B => Unit] =
      ConfigDef.need(k)(r).map(a => f(_)(a))

    def getC[A](k: String, f: (B, A) => Unit)(implicit r: ValueParser[A]): ConfigDef[B => Unit] =
      get[A](k, f.curried)

    def getOrUseC[A](k: String, f: (B, A) => Unit)(default: A)(implicit r: ValueParser[A]): ConfigDef[B => Unit] =
      getOrUse[A](k, f.curried)(default)

    def needC[A](k: String, f: (B, A) => Unit)(implicit r: ValueParser[A]): ConfigDef[B => Unit] =
      need[A](k, f.curried)

    def apply(cs: (ConsumerFn[B] => ConfigDef[B => Unit])*): ConfigDef[B => Unit] =
      mergeConsumerFns(cs.map(_ apply this): _*)
  }


  def mergeConsumerFns[A](cs: ConfigDef[A => Unit]*): ConfigDef[A => Unit] =
    if (cs.isEmpty)
      ((_: A) => ()).pure[ConfigDef]
    else
      cs.reduce(applicativeInstance.map2(_, _)((f, g) => a => { f(a); g(a) }))

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  implicit val applicativeInstance: Applicative[ConfigDef] =
    new Applicative[ConfigDef] {
      override def pure[A](a: A)                                     = ConfigDef.const(a)
      override def map[A, B](fa: ConfigDef[A])(f: A => B)            = fa map f
      override def ap[A, B](ff: ConfigDef[A => B])(fa: ConfigDef[A]) = fa.ap(ff)
    }

  private implicit def configToInstance[A](c: ConfigDef[A]): Instance[A] =
    c match {
      case i: Instance[A] => i
    }

  private trait Instance[A] extends ConfigDef[A] { self =>
    def step[F[_]](implicit F: Monad[F]): Step[F, StepResult[A]]

    override final def ap[B](ff: ConfigDef[A => B]): ConfigDef[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]) = {
          // type X[Y] = F[(R[F], S[F]) => F[(Unit, StepResult[Y], S[F])]]
          val xa = self.step(F).runF // : X[A]
          val xf = ff.step(F).runF // : X[A => B]
          Step[F, StepResult[B]] { (r, s0) =>
            for {
              gf          <- xf
              ga          <- xa
              hf          <- gf(r, s0)
              (_, s1, ff) = hf
              ha          <- ga(r, s1)
              (_, s2, fa) = ha
            } yield (s2, StepResult.catsInstance.ap(ff)(fa))
          }
        }
      }

    override final def run[F[_]](sources: Sources[F])(implicit pp: ValuePreprocessor, F: Monad[F]): F[Result[A]] = {
      type OK = (SourceName, Store[F])
      type KO = (SourceName, String)

      // Prepare sources
      val r1: Vector[F[Either[KO, OK]]] = sources.highToLowPri.map(s => F.map(s.prepare)(_.bimap(s.name -> _, s.name -> _.mapValues(pp.run))))
      val r2: F[Vector[Either[KO, OK]]] = r1.sequence
      val r3: F[Either[KO, Vector[OK]]] = F.map(r2)(_.sequence)

      F.flatMap(r3) {
        case Right(stores) =>

          // Read config
          step(F).run(R(stores), S.init).map { case (_, _, stepResult) =>
            stepResult match {
              case StepResult.Success(a, _) => Result.Success(a)
              case StepResult.Failure(x, y) => Result.QueryFailure(x, y.map(_.public), sources.highToLowPri.map(_.name))
            }
          }

        case Left((s, err)) => F.pure(Result.PreparationFailure(s, err))
      }
    }

    override final def map[B](f: A => B): ConfigDef[B] =
      stepMap(ra => ra.flatMap(a => StepResult.Success(f(a), Set(Origin.Map))))

    override final def mapAttempt[B](f: A => Either[String, B]): ConfigDef[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]) =
          self.step(F) map {
            case StepResult.Success(a, originsA) =>
              f(a) match {
                case Right(b) => StepResult.Success(b, originsA)
                case Left(e) => StepResult.fail(e, originsA)
              }
            case f: StepResult.Failure => f
          }
      }

    final def stepMap[B](f: StepResult[A] => StepResult[B]): ConfigDef[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]) =
          self.step(F).map(f)
      }

    override final def chooseAttempt[B](f: A => Either[String, ConfigDef[B]]): ConfigDef[B] =
      new Instance[B] {
        def step[F[_]](implicit F: Monad[F]): Step[F, StepResult[B]] =
          self.step(F).flatMap {
            case StepResult.Success(a, originsA) =>
              f(a) match {
                case Right(cb) => cb.step(F)
                case Left(e)  => Step.ret(StepResult.fail(e, originsA))
              }
            case f: StepResult.Failure => Step.ret(f)
          }
      }

    override final def option: ConfigDef[Option[A]] =
      new Instance[Option[A]] {
        def step[F[_]](implicit F: Monad[F]) = {
          val get = Step.get[F]

          def transform(s1: S[F], s2: S[F], sr: StepResult[A]): F[StepResult[Option[A]]] = {
            def noKeysSpecifiedF: F[Boolean] =
              s2.queriesSince(s1).foldLeftM(true)((ok, k) => {
                if (ok)
                  s2.queryCache(k).selected.map {
                    case None | Some(SrcAndVal(_, Lookup.NotFound)) => true
                    case Some(SrcAndVal(_, _: Lookup.Found | _: Lookup.Error)) => false
                  }
                else
                  F pure false
              })

            sr match {
              case StepResult.Success(a, o) =>
                noKeysSpecifiedF.map(noKeysSpecified =>
                  if (noKeysSpecified)
                    StepResult.Success(None, Set.empty)
                  else
                    StepResult.Success(Some(a), o))

              case f@StepResult.Failure(keyed, other) =>
                def noErrors = keyed.valuesIterator.forall(_.isEmpty)
                noKeysSpecifiedF.map(noKeysSpecified =>
                  if (noKeysSpecified && noErrors && other.isEmpty)
                    StepResult.Success(None, Set.empty)
                  else
                    f)
            }
          }

          for {
            s1 <- get
            r1 <- self.step(F)
            s2 <- get
            r2 <- Step.retF(transform(s1, s2, r1))
          } yield r2
        }
      }

    override final def secret: ConfigDef[A] =
      new Instance[A] {
        def step[F[_]](implicit F: Monad[F]) = {
          for {
            s <- Step.get[F]
            r <- self.step(F)
            _ <- Step.modify[F](s2 => s2.obfuscateKeys(s2.queriesSince(s)))
          } yield r
        }
      }

    override final def withKeyMod(f: String => String): ConfigDef[A] =
      Instance.keyModCompose(f) *> this <* Instance.keyModPop

    override final def withReport(implicit s: Report.Settings): ConfigDef[(A, Report)] =
      ((this: ConfigDef[A]), ConfigDef.Instance.reportSoFar(s)).tupled
  }

  private object Instance {
    private def keyModTS(f: Key => Key): String => String = s => f(Key(s)).value
    private def keyModFS(f: String => String): Key => Key = k => Key(f(k.value))

    def baseGet[A](key: String, apiMethod: ApiMethod, doIt: (Key, Option[Origin.Read]) => StepResult[A]): ConfigDef[A] =
      new Instance[A] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step { (r, s1) =>
            val k = s1.keyMod(Key(key))

            val (vo: QueryResult[F], s2) =
              s1.queryCache.get(k) match {
                case Some(q) => (q, s1)

                case None =>
                  val results: F[Vector[SrcAndVal]] =
                    r.highToLowPri.iterator
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

    def baseGetA[A, B](key: String, apiMethod: ApiMethod, doIt: (Key, Option[(Origin.Read, A)]) => StepResult[B])(implicit v: ValueParser[A]): ConfigDef[B] =
      baseGet(key, apiMethod, (k, oo) =>
        oo match {
          case Some(o) =>
            v.parse(o.sourceValue.value) match {
              case Right(a) => doIt(k, Some((o, a)))
              case Left(e) => StepResult.Failure(Map(k -> Some((o.sourceName, Lookup.Error(e, Some(o.sourceValue.value))))), Set.empty)
            }
          case None => doIt(k, None)
        }
      )

    def keyModUpdate(f: (String => String) => String => String): ConfigDef[Unit] =
      new Instance[Unit] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step((_, s) => (s.keyModPush(keyModFS(f(keyModTS(s.keyMod)))), ().pure[StepResult]).pure[F])
      }

    def keyModCompose(f: String => String): ConfigDef[Unit] =
      keyModUpdate(_ compose f)

    def keyModPop: ConfigDef[Unit] =
      new Instance[Unit] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step((_, s) => (s.keyModPop, ().pure[StepResult]).pure[F])
      }

    def keysUsed: ConfigDef[Set[Key]] =
      new Instance[Set[Key]] {
        override def step[F[_]](implicit F: Monad[F]) =
          Step((_, s) => (s, s.queryCache.keySet.pure[StepResult]).pure[F])
      }

    def reportSoFar(settings: Report.Settings): ConfigDef[Report] =
      new Instance[Report] {
        def step[F[_]](implicit F: Monad[F]) =
          Step((r, s) => ReportCreation(settings, r, s).map(a => (s, a.pure[StepResult])))
      }

  }
}
