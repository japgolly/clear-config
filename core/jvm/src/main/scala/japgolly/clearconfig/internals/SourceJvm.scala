package japgolly.clearconfig.internals

import java.io.{File, FileInputStream}
import scalaz.{-\/, Applicative, \/, \/-}

object SourceNameJvm extends SourceNameObjectJvm
trait SourceNameObjectJvm extends SourceNameObject {
  final def classpath(filename: String) = SourceName(s"cp:$filename")
  final def environment = SourceName("Env")
  final def system = SourceName("System")
}

object SourceJvm extends SourceObjectJvm
trait SourceObjectJvm extends SourceObject {

  def environment[F[_]](implicit F: Applicative[F]): Source[F] =
    environment(true)

  def environment[F[_]](replaceDotsWithUnderscores: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val s = point(SourceNameJvm.environment.value, StoreJvm.ofMap(sys.env))
    if (replaceDotsWithUnderscores)
      s.mapKeyQueries(k => k :: k.replace('.', '_') :: Nil)
    else
      s
  }

  def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](SourceNameJvm.system, F.point {
      def cfg() = StoreJvm.ofJavaProps[F](System.getProperties())
      \/.fromTryCatchNonFatal(cfg()).leftMap(_.getMessage)
    })

  def propFileOnClasspath[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val f = filename.replaceFirst("^/*", "/")
    Source[F](SourceNameJvm.classpath(f), F.point {
      def load() = {
        val i = getClass.getResourceAsStream(f)
        if (i ne null)
          \/-(StoreJvm.ofJavaPropsFromInputStream[F](i, close = true))
        else if (optional)
          \/-(StoreJvm.empty[F])
        else
          -\/("File not found.")
      }
      \/.fromTryCatchNonFatal(load()).leftMap(_.getMessage).flatMap(identity)
    })
  }

  def propFile[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] =
    propFile(new File(filename), optional)

  def propFile[F[_]](file: File, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    Source[F](SourceName(file.getAbsolutePath), F.point {
      def load() =
        if (file.exists()) {
          if (file.canRead) {
            val is = new FileInputStream(file)
            \/-(StoreJvm.ofJavaPropsFromInputStream[F](is, close = true))
          } else
            -\/("Unable to read file.")
        } else if (optional)
          \/-(StoreJvm.empty[F])
        else
          -\/("File not found.")
      \/.fromTryCatchNonFatal(load()).leftMap(_.getMessage).flatMap(identity)
    })
  }

}
