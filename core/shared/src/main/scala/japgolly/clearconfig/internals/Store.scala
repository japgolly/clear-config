package japgolly.clearconfig.internals

import scalaz.{Applicative, Functor, ~>}
import scalaz.std.list._
import scalaz.syntax.traverse._
import japgolly.microlibs.stdlib_ext.StdlibExt._

trait Store[F[_]] { self =>

  def apply(key: Key): F[Lookup]
  val all: F[Map[Key, String]]

  /** Expands each key query into multiple, and chooses the first that returns a result. */
  def mapKeyQueries(f: Key => List[Key])(implicit F: Applicative[F]): Store[F] =
    new Store[F] {
      override def apply(origKey: Key) =
        f(origKey)
          .traverse(self.apply)
          .map(_.find(_ != Lookup.NotFound) getOrElse Lookup.NotFound)
      override val all = self.all
      override def toString = self.toString + "*"
      override def hashCode = self.##
    }

  def mapValues(f: String => String)(implicit F: Functor[F]): Store[F] =
    mapValues((_, v) => f(v))

  def mapValues(f: (Key, String) => String)(implicit F: Functor[F]): Store[F] =
    new Store[F] {
      override def apply(key: Key) =
        self(key).map {
          case Lookup.Found(k, v) => Lookup.Found(k, f(k, v))
          case l@(Lookup.NotFound | Lookup.Error(_, _)) => l
        }
      override val all = self.all.map(_.mapEntriesNow((k, v) => k -> f(k, v)))
      override def toString = self.toString
      override def hashCode = self.##
    }

  def trans[G[_]](t: F ~> G): Store[G] =
    new Store[G] {
      override def apply(key: Key) = t(self(key))
      override val all = t(self.all)
      override def toString = self.toString + ".trans"
      override def hashCode = self.##
    }
}

trait StoreObject {
  protected final def objName = "ConfigStore"

  final def empty[F[_]](implicit F: Applicative[F]): Store[F] =
    new Store[F] {
      override def toString = s"$objName.empty"
      override def hashCode = 0
      override def apply(key: Key) = F.pure(Lookup.NotFound)
      override val all = F.pure(Map.empty)
    }

  final def ofMap[F[_]](m: Map[String, String])(implicit F: Applicative[F]): Store[F] =
    new Store[F] {
      override def toString = s"$objName.ofMap($m)"
      override def hashCode = m.##
      override def apply(key: Key) = {
        val o = m.get(key.value)
        val r = Lookup.fromOption(key, o)
        F.pure(r)
      }
      override val all =
        F.pure(m.mapKeysNow(Key))
    }
}

object StoreObject extends StoreObject