package japgolly.clearconfig.internals

import cats.Applicative
import cats.syntax.all._
import java.io.{File, FileInputStream}

object SourceNameJvm extends SourceNameObjectJvm
trait SourceNameObjectJvm extends SourceNameObject {
  final def classpath(filename: String) = SourceName(s"cp:$filename")
}

object SourceJvm extends SourceObjectJvm
trait SourceObjectJvm extends SourceObject {

  final def propFileOnClasspath[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    val f = filename.replaceFirst("^/*", "/")
    Source[F](SourceNameJvm.classpath(f), F.point {
      def load() = {
        val i = getClass.getResourceAsStream(f)
        if (i ne null)
          Right(StoreJvm.ofJavaPropsFromInputStream[F](i, close = true))
        else if (optional)
          Right(StoreJvm.empty[F])
        else
          Left("File not found.")
      }
      Util.eitherTry(load()).leftMap(_.getMessage).flatMap(identity)
    })
  }

  final def propFile[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] =
    propFile(new File(filename), optional)

  final def propFile[F[_]](file: File, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
    Source[F](SourceName(file.getAbsolutePath), F.point {
      def load() =
        if (file.exists()) {
          if (file.canRead) {
            val is = new FileInputStream(file)
            Right(StoreJvm.ofJavaPropsFromInputStream[F](is, close = true))
          } else
            Left("Unable to read file.")
        } else if (optional)
          Right(StoreJvm.empty[F])
        else
          Left("File not found.")
      Util.eitherTry(load()).leftMap(_.getMessage).flatMap(identity)
    })
  }
}
