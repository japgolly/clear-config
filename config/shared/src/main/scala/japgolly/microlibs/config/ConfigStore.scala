package japgolly.microlibs.config

import java.util.Properties
import scala.collection.JavaConverters._
import scalaz.{Applicative, Monad, ~>}

sealed trait ConfigValue extends Product with Serializable
object ConfigValue {
  final case class Found(key: Key, value: String) extends ConfigValue
  case object NotFound extends ConfigValue
  final case class Error(desc: String, value: Option[String]) extends ConfigValue

  @inline def notFound: ConfigValue = NotFound

  def fromOption(k: Key, o: Option[String]): ConfigValue =
    o match {
      case Some(v) => Found(k, v)
      case None => NotFound
    }
}

trait ConfigStore[F[_]] {
  def apply(key: Key): F[ConfigValue]
  def getBulk(filter: Key => Boolean): F[Map[Key, String]]

  /** Expands each key query into multiple, and chooses the first that returns a result. */
  def mapKeyQueries(f: Key => List[Key])(implicit F: Monad[F]): ConfigStore[F] = {
    val self = this
    new ConfigStore[F] {
      override def apply(origKey: Key) =
        f(origKey).foldLeft(F pure ConfigValue.notFound)((plan, key) =>
          F.bind(plan) {
            case ConfigValue.NotFound => self(key)
            case other                => F pure other
          }
        )
      override def getBulk(f: Key => Boolean) = self.getBulk(f)
      override def toString = s"$self*"
      override def hashCode = self.##
    }
  }

  def trans[G[_]](t: F ~> G): ConfigStore[G] = {
    val self = this
    new ConfigStore[G] {
      override def apply(key: Key) = t(self(key))
      override def getBulk(f: Key => Boolean) = t(self.getBulk(f))
    }
  }
}

object ConfigStore {
  private def obj = "ConfigStore"

  def empty[F[_]](implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.empty"
      override def hashCode = 0
      override def apply(key: Key) = F.pure(ConfigValue.NotFound)
      override def getBulk(f: Key => Boolean) = F.pure(Map.empty)
    }

  def javaProps[F[_]](p: Properties)(implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.javaProps($p)"
      override def hashCode = p.##
      override def apply(key: Key) = {
        val o = Option(p.getProperty(key.value))
        val r = ConfigValue.fromOption(key, o)
        F.pure(r)
      }
      override def getBulk(f: Key => Boolean) = F.pure(
        p.keys()
          .asScala
          .map(k => Key(k.toString))
          .filter(f)
          .map(k => k -> p.getProperty(k.value))
          .toMap)
    }

  def stringMap[F[_]](m: Map[String, String])(implicit F: Applicative[F]): ConfigStore[F] =
    new ConfigStore[F] {
      override def toString = s"$obj.stringMap($m)"
      override def hashCode = m.##
      override def apply(key: Key) = {
        val o = m.get(key.value)
        val r = ConfigValue.fromOption(key, o)
        F.pure(r)
      }
      override def getBulk(f: Key => Boolean) = F.pure(m.toIterator
        .map { case (k, v) => (Key(k), v) }
        .filter(x => f(x._1))
        .toMap)
    }
}
