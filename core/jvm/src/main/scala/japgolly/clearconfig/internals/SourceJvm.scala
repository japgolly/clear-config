package japgolly.clearconfig.internals

import cats.syntax.all._
import cats.{Applicative, Monad}
import java.io.{ByteArrayInputStream, File, FileInputStream}

object SourceNameJvm extends SourceNameObjectJvm
trait SourceNameObjectJvm extends SourceNameObject {
  final def classpath(filename: String) = SourceName(s"cp:$filename")
  final def system = SourceName("System")
}

object SourceJvm extends SourceObjectJvm
trait SourceObjectJvm extends SourceObject {

  override protected def envStore[F[_]](implicit F: Applicative[F]): Store[F] =
    StoreJvm.ofMap(sys.env)

  def system[F[_]](implicit F: Applicative[F]): Source[F] =
    Source[F](SourceNameJvm.system, F.point {
      def cfg() = StoreJvm.ofJavaProps[F](System.getProperties())
      Util.eitherTry(cfg()).leftMap(_.getMessage)
    })

  def propFileOnClasspath[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
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

  def propFile[F[_]](filename: String, optional: Boolean)(implicit F: Applicative[F]): Source[F] =
    propFile(new File(filename), optional)

  def propFile[F[_]](file: File, optional: Boolean)(implicit F: Applicative[F]): Source[F] = {
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
  def expandInlineProperties[F[_]](source: Source[F], key: String)(implicit F: Monad[F]): Source[F] = {
    val k = Key(key)

    val prepare2: F[Either[String, Store[F]]] =
      source.prepare.flatMap {

        case Right(store) =>
          store.all.flatMap { map =>
            map.get(k) match {

              case Some(value) =>
                val is = new ByteArrayInputStream(value.getBytes("UTF-8"))
                val store2 = StoreJvm.ofJavaPropsFromInputStream[F](is, close = true)
                for {
                  map2 <- store2.all
                } yield {
                  val common = (map.keySet & map2.keySet).filter(k => map(k) != map2(k))
                  if (common.nonEmpty) {
                    val hdr = s"The following keys are defined at both the top-level and in ${k.value}: "
                    val keys = common.iterator.map(_.value).toList.sorted
                    Left(keys.mkString(hdr, ", ", "."))
                  } else
                    Right(StoreObject[F](F.pure(map ++ map2 - k)))
                }

              case None =>
                F.pure(Right(store))
            }
          }

        case e@ Left(_) =>
          F.pure(e)
      }

    source.copy(prepare = prepare2)
  }
}
