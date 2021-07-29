package japgolly.clearconfig.internals

import cats.syntax.either._
import cats.{Applicative, ~>}

final case class Source[F[_]](name: SourceName, prepare: F[Either[String, Store[F]]])(implicit F: Applicative[F]) {
  override def toString: String =
    s"Source(${name.value})"

  def withSuffix(s: String): Source[F] =
    copy(name.withSuffix(s))

  def toSources: Sources[F] =
    Sources(Vector.empty :+ this)

  def mapStore(f: Store[F] => Store[F]): Source[F] =
    copy(prepare = F.map(prepare)(_.map(f)))

  /** Expands each key query into multiple, and chooses the first that returns a result. */
  def mapKeyQueries(f: Key => List[Key]): Source[F] =
    mapStore(_.mapKeyQueries(f))

  def mapValues(f: String => String): Source[F] =
    mapStore(_.mapValues(f))

  def mapValues(f: (Key, String) => String): Source[F] =
    mapStore(_.mapValues(f))

  def normaliseKeys(f: Key => Key): Source[F] =
    mapStore(_.normaliseKeys(f))

  def caseInsensitive: Source[F] =
    mapStore(_.caseInsensitive)

  def trans[G[_]](t: F ~> G)(implicit G: Applicative[G]): Source[G] =
    copy(prepare = G.map(t(prepare))(_.map(_ trans t)))
}

trait SourceObject {

  final def apply[F[_]](name: SourceName, prepare: F[Either[String, Store[F]]])(implicit F: Applicative[F]): Source[F] =
    Source(name, prepare)(F)

  final def point[F[_]](name: String, store: => Store[F])(implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName(name), F.point(Right(store)))

  final def empty[F[_]](name: String)(implicit F: Applicative[F]): Source[F] =
    manual(name)()

  final def manual[F[_]](name: String)(kvs: (String, String)*)(implicit F: Applicative[F]): Source[F] =
    manual(name, kvs.toMap)

  final def manual[F[_]](name: String, kvs: Map[String, String])(implicit F: Applicative[F]): Source[F] =
    point(name, StoreObject.ofMap(kvs))

  final def environment[F[_]](implicit F: Applicative[F]): Source[F] =
    environment(true)

  final def environment[F[_]](replaceDotsWithUnderscores: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val s = point(SourceName.environment.value, envStore[F])
    if (replaceDotsWithUnderscores)
      s.mapKeyQueries(k => k :: k.replace('.', '_') :: Nil)
    else
      s
  }

  protected def envStore[F[_]](implicit F: Applicative[F]): Store[F]

  final def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName.system, F.point {
      def cfg() = StoreObject.ofJavaProps[F](System.getProperties())
      Util.eitherTry(cfg()).leftMap(_.getMessage)
    })
}
