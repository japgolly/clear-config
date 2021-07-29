package japgolly.clearconfig.internals

import cats.instances.list._
import cats.syntax.all._
import cats.{Applicative, ~>}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.io.InputStream
import java.util.Properties
import scala.jdk.CollectionConverters._

final class Store[F[_]](val all : F[Map[Key, String]],
                        lookup  : Key => F[Lookup],
                        queryMap: Key => List[Key],
                        keyNorm: Option[Key => Key])(implicit F: Applicative[F]) {self =>

  def apply(key: Key): F[Lookup] =
    keyNorm match {
      case None =>
        queryMap(key)
          .traverse(lookup)
          .map(_.find(_ != Lookup.NotFound) getOrElse Lookup.NotFound)

      case Some(n) =>
        all.map(m =>
          // quadratic but will never be large
          queryMap(key)
            .iterator
            .map(n)
            .map(q => m.find(kv => n(kv._1) == q))
            .firstDefined match {
            case Some((k, v)) => Lookup.Found(k, v)
            case None => Lookup.NotFound
          }
        )
    }

  private def copy(all: F[Map[Key, String]] = self.all,
                   lookup: Key => F[Lookup] = self.lookup,
                   queryMap: Key => List[Key] = self.queryMap,
                   keyNorm: Option[Key => Key] = self.keyNorm): Store[F] =
    new Store(all, lookup, queryMap, keyNorm)

  /** Expands each key query into multiple, and chooses the first that returns a result. */
  def mapKeyQueries(f: Key => List[Key]): Store[F] =
    copy(queryMap = queryMap(_).flatMap(f))

  def mapValues(f: String => String): Store[F] =
    mapValues((_, v) => f(v))

  def mapValues(f: (Key, String) => String): Store[F] =
    copy(
      lookup = self(_).map {
        case Lookup.Found(k, v) => Lookup.Found(k, f(k, v))
        case l@(Lookup.NotFound | _: Lookup.Error) => l
      },
      all = self.all.map(_.mapEntriesNow((k, v) => k -> f(k, v))))

  def normaliseKeys(f: Key => Key): Store[F] =
    copy(keyNorm = Some(keyNorm.fold(f)(f.compose)))

  def caseInsensitive: Store[F] =
    normaliseKeys(_.toLowerCase)

  def trans[G[_]: Applicative](t: F ~> G): Store[G] =
    new Store(t(all), lookup.andThen(t.apply), queryMap, keyNorm)
}

trait StoreObject {
  protected final def objName = "ConfigStore"

  final def apply[F[_]](all: F[Map[Key, String]])(implicit F: Applicative[F]): Store[F] =
    apply(all, k => all.map(m => Lookup.fromOption(k, m get k)))

  final def apply[F[_]](all: F[Map[Key, String]],
                  lookup: Key => F[Lookup])(implicit F: Applicative[F]): Store[F] =
    new Store(all, lookup, _ :: Nil, None)

  final def empty[F[_]](implicit F: Applicative[F]): Store[F] =
    apply(F.pure(Map.empty))

  final def ofMap[F[_]](m: => Map[String, String])(implicit F: Applicative[F]): Store[F] = {
    lazy val m2 = m.mapKeysNow(Key.apply)
    apply(F.point(m2))
  }

  private def propsToMap(p: Properties): Map[Key, String] =
    p.keys().asScala.map { kx =>
      val k = "" + kx
      Key(k) -> p.getProperty(k)
    }.toMap

  final def ofJavaProps[F[_]](p: Properties)(implicit F: Applicative[F]): Store[F] = {
    lazy val m = propsToMap(p)
    apply(F.point(m))
  }

  final def ofJavaPropsFromInputStream[F[_]](is: InputStream, close: Boolean = true)(implicit F: Applicative[F]): Store[F] = {
    lazy val m =
      try {
        val p = new Properties()
        p.load(is)
        propsToMap(p)
      } finally
        if (close)
          is.close()
    apply(F.point(m))
  }
}

object StoreObject extends StoreObject
