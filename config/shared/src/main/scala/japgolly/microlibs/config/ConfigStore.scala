package japgolly.microlibs.config

import java.util.Properties
import scala.collection.JavaConverters._
import scalaz.Applicative

sealed trait ConfigValue extends Product with Serializable
object ConfigValue {
  final case class Found(value: String) extends ConfigValue
  case object NotFound extends ConfigValue
  final case class Error(desc: String, value: Option[String]) extends ConfigValue

  def option(o: Option[String]): ConfigValue =
    o match {
      case Some(v) => Found(v)
      case None => NotFound
    }
}

trait ConfigStore[F[_]] {
  def apply(key: Key): F[ConfigValue]
  def getBulk(filter: Key => Boolean): F[Map[Key, String]]
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
        val r = ConfigValue.option(o)
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
        val r = ConfigValue.option(o)
        F.pure(r)
      }
      override def getBulk(f: Key => Boolean) = F.pure(m.toIterator
        .map { case (k, v) => (Key(k), v) }
        .filter(x => f(x._1))
        .toMap)
    }
}

