package japgolly.clearconfig.internals

import java.io.InputStream
import java.util.Properties
import scala.collection.JavaConverters._
import scalaz.Applicative

object StoreJvm extends StoreObjectJvm
trait StoreObjectJvm extends StoreObject {

  final def ofJavaProps[F[_]](p: Properties)(implicit F: Applicative[F]): Store[F] =
    new Store[F] {
      override def toString = s"$objName.javaProps($p)"
      override def hashCode = p.##
      override def apply(key: Key) = {
        val o = Option(p.getProperty(key.value))
        val r = Lookup.fromOption(key, o)
        F.pure(r)
      }
      override val all =
        F.pure(
          p.keys()
            .asScala
            .map(k => Key(k.toString))
            .map(k => k -> p.getProperty(k.value))
            .toMap)
    }

  final def ofJavaPropsFromInputStream[F[_]](is: InputStream, close: Boolean = true)(implicit F: Applicative[F]): Store[F] =
    try {
      val p = new Properties()
      p.load(is)
      ofJavaProps[F](p)
    } finally
      if (close)
        is.close()

}
