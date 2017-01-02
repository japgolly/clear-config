package japgolly.microlibs.config

import java.util.Properties
import scalaz.{Applicative, -\/, \/, \/-}

final case class SourceName(value: String) extends AnyVal

final case class Source[F[_]](name: SourceName, prepare: F[String \/ ConfigStore[F]]) {
  override def toString: String = s"Source(${name.value})"
}

object Source {
  implicit def toSources[F[_]](s: Source[F]): Sources[F] =
    Sources(Vector.empty :+ s)

  def point[F[_]](name: String, config: => ConfigStore[F])(implicit F: Applicative[F]): Source[F] =
    Source[F](SourceName(name), F.point(\/-(config)))

  def manual[F[_]](name: String)(kvs: (String, String)*)(implicit F: Applicative[F]): Source[F] =
    point(name, ConfigStore.stringMap(kvs.toMap))

  def environmentName = SourceName("Environment")

  def environment[F[_]](implicit F: Applicative[F]): Source[F] =
    point(environmentName.value, ConfigStore.stringMap(sys.env))

  def systemName = SourceName("System")

  // TODO move names to SourceName.xxx
  def applicativePointName = SourceName("Manual")

  def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](systemName, F.point {
      def cfg() = ConfigStore.javaProps[F](System.getProperties())
      \/.fromTryCatchNonFatal(cfg()).leftMap(_.getMessage)
    })

  def propFileOnClasspath[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val f = filename.replaceFirst("^/*", "/")
    Source[F](SourceName(s"classpath:$f"), F.point {
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
  override def toString: String = s"Sources(${highToLowPri.map(_.name.value) mkString " > "})"

  def >(lowerPri: Sources[F]): Sources[F] =
    Sources(highToLowPri ++ lowerPri.highToLowPri)

  def <(higherPri: Sources[F]): Sources[F] =
    higherPri > this
}

object Sources {
  def empty[F[_]]: Sources[F] =
    apply(Vector.empty)
}
