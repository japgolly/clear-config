package japgolly.microlibs.config

import java.util.Properties
import scalaz.{-\/, Applicative, Functor, Monad, \/, \/-, ~>}

final case class SourceName(value: String) extends AnyVal
object SourceName {
  def classpath(filename: String) = SourceName(s"cp:$filename")
  def environment = SourceName("Env")
  def system = SourceName("System")
  def api = SourceName("API")
}

final case class Source[F[_]](name: SourceName, prepare: F[String \/ ConfigStore[F]]) {
  override def toString: String =
    s"Source(${name.value})"

  def toSources: Sources[F] =
    Sources(Vector.empty :+ this)

  /** Expands each key query into multiple, and chooses the first that returns a result. */
  def mapKeyQueries(f: Key => List[Key])(implicit F: Monad[F]): Source[F] =
    Source(name, F.map(prepare)(_.map(_.mapKeyQueries(f)(F))))

  def trans[G[_]](t: F ~> G)(implicit G: Functor[G]): Source[G] =
    copy(prepare = G.map(t(prepare))(_.map(_ trans t)))
}

object Source {
  implicit def toSources[F[_]](s: Source[F]): Sources[F] =
    s.toSources

  implicit def seqToSources[F[_]](s: Seq[Source[F]]): Seq[Sources[F]] =
    s.map(_.toSources)

  def point[F[_]](name: String, config: => ConfigStore[F])(implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName(name), F.point(\/-(config)))

  def empty[F[_]](name: String)(implicit F: Applicative[F]): Source[F] =
    manual(name)()

  def manual[F[_]](name: String)(kvs: (String, String)*)(implicit F: Applicative[F]): Source[F] =
    manual(name, kvs.toMap)

  def manual[F[_]](name: String, kvs: Map[String, String])(implicit F: Applicative[F]): Source[F] =
    point(name, ConfigStore.stringMap(kvs))

  def environment[F[_]](implicit F: Applicative[F]): Source[F] =
    point(SourceName.environment.value, ConfigStore.stringMap(sys.env))

  def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName.system, F.point {
      def cfg() = ConfigStore.javaProps[F](System.getProperties())
      \/.fromTryCatchNonFatal(cfg()).leftMap(_.getMessage)
    })

  def propFileOnClasspath[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val f = filename.replaceFirst("^/*", "/")
    Source[F](SourceName.classpath(f), F.point {
      def load() = {
        val i = getClass.getResourceAsStream(f)
        if (i eq null) {
          if (optional)
            \/-(ConfigStore.empty[F])
          else
            -\/("File not found.")
        } else {
          val p = new Properties()
          p.load(i)
          \/-(ConfigStore.javaProps[F](p))
        }
      }
      \/.fromTryCatchNonFatal(load()).leftMap(_.getMessage).flatMap(identity)
    })
  }
}

final case class Sources[F[_]](highToLowPri: Vector[Source[F]]) extends AnyVal {
  override def toString: String =
    s"Sources(${highToLowPri.map(_.name.value) mkString " > "})"

  def >(lowerPri: Sources[F]): Sources[F] =
    Sources(highToLowPri ++ lowerPri.highToLowPri)

  def <(higherPri: Sources[F]): Sources[F] =
    higherPri > this

  def reverse: Sources[F] =
    Sources(highToLowPri.reverse)

  def trans[G[_]: Functor](f: F ~> G): Sources[G] =
    Sources(highToLowPri.map(_ trans f))
}

object Sources {
  def empty[F[_]]: Sources[F] =
    apply(Vector.empty)

  def highToLowPri[F[_]](ss: Sources[F]*): Sources[F] =
    apply(ss.iterator.flatMap(_.highToLowPri).toVector)

  def lowToHighPri[F[_]](ss: Sources[F]*): Sources[F] =
    highToLowPri(ss.reverse: _*)
}
