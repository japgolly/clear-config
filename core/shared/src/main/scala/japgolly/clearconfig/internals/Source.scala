package japgolly.clearconfig.internals

import cats.syntax.all._
import cats.{Applicative, Monad, ~>}
import java.io.ByteArrayInputStream

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

  /** If a value is provided at the specified key, treat it as the contents of a properties file, expand it by merging
    * it with all the top-level properties, then remove it.
    *
    * Eg. if you expand the "INLINE" key below
    *
    * {{{
    *   A = 1
    *   INLINE = B = 2
    *            C = 3
    * }}}
    *
    * then you get the following result
    *
    * {{{
    *   A = 1
    *   B = 2
    *   C = 3
    * }}}
    */
  def expandInlineProperties(key: String)(implicit F: Monad[F]): Source[F] = {
    val k = Key(key)

    val prepare2: F[Either[String, Store[F]]] =
      prepare.flatMap {

        case Right(store) =>
          store.all.flatMap { map =>
            map.get(k) match {

              case Some(value) =>
                val is = new ByteArrayInputStream(value.getBytes("UTF-8"))
                val store2 = Platform.ConfigStore.ofJavaPropsFromInputStream[F](is, close = true)
                for {
                  map2 <- store2.all
                } yield {
                  val common = (map.keySet & map2.keySet).filter(k => map(k) != map2(k))
                  if (common.nonEmpty) {
                    val hdr = s"The following keys are defined at both the top-level and in ${k.value}: "
                    val keys = common.iterator.map(_.value).toList.sorted
                    Left(keys.mkString(hdr, ", ", "."))
                  } else
                    Right(Platform.ConfigStore[F](F.pure(map ++ map2 - k)))
                }

              case None =>
                F.pure(Right(store))
            }
          }

        case e@ Left(_) =>
          F.pure(e)
      }

    copy(prepare = prepare2)
  }

}

trait SourceObject {
  import Platform.ConfigStore

  final def apply[F[_]](name: SourceName, prepare: F[Either[String, Store[F]]])(implicit F: Applicative[F]): Source[F] =
    Source(name, prepare)(F)

  final def point[F[_]](name: String, store: => Store[F])(implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName(name), F.point(Right(store)))

  final def empty[F[_]](name: String)(implicit F: Applicative[F]): Source[F] =
    manual(name)()

  final def manual[F[_]](name: String)(kvs: (String, String)*)(implicit F: Applicative[F]): Source[F] =
    manual(name, kvs.toMap)

  final def manual[F[_]](name: String, kvs: Map[String, String])(implicit F: Applicative[F]): Source[F] =
    point(name, ConfigStore.ofMap(kvs))

  final def environment[F[_]](implicit F: Applicative[F]): Source[F] =
    environment(true)

  final def environment[F[_]](replaceDotsWithUnderscores: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val s = point(SourceName.environment.value, ConfigStore.environment[F])
    if (replaceDotsWithUnderscores)
      s.mapKeyQueries(k => k :: k.replace('.', '_') :: Nil)
    else
      s
  }

  final def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName.system, F.point {
      def cfg() = ConfigStore.ofJavaProps[F](System.getProperties())
      Util.eitherTry(cfg()).leftMap(_.getMessage)
    })
}
