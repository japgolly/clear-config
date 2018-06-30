package japgolly.clearconfig.internals

import java.io.InputStream
import java.util.Properties
import scala.collection.JavaConverters._
import scalaz.Applicative

object StoreJvm extends StoreObjectJvm
trait StoreObjectJvm extends StoreObject {

  private def propsToMap(p: Properties): Map[Key, String] =
    p.keys().asScala.map { kx =>
      val k = kx.toString
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
