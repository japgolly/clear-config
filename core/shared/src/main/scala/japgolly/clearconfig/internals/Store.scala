package japgolly.clearconfig.internals

import scalaz.{Applicative, Functor, Monad, ~>}
import scalaz.syntax.functor._
import japgolly.microlibs.stdlib_ext.StdlibExt._

trait Store[F[_]] { self =>

  def apply(key: Key): F[Lookup]
  def getBulk(filter: Key => Boolean): F[Map[Key, String]]

  /** Expands each key query into multiple, and chooses the first that returns a result. */
  def mapKeyQueries(f: Key => List[Key])(implicit F: Monad[F]): Store[F] =
    new Store[F] {
      override def apply(origKey: Key) =
        f(origKey).foldLeft(F pure Lookup.notFound)((plan, key) =>
          F.bind(plan) {
            case Lookup.NotFound => self(key)
            case other           => F pure other
          }
        )
      override def getBulk(f: Key => Boolean) = self.getBulk(f)
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
      override def getBulk(g: Key => Boolean) =
        self.getBulk(g).map(_.mapEntriesNow((k, v) => k -> f(k, v)))
      override def toString = self.toString
      override def hashCode = self.##
    }

  def trans[G[_]](t: F ~> G): Store[G] =
    new Store[G] {
      override def apply(key: Key) = t(self(key))
      override def getBulk(f: Key => Boolean) = t(self.getBulk(f))
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
      override def getBulk(f: Key => Boolean) = F.pure(Map.empty)
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
      override def getBulk(f: Key => Boolean) =
        F.pure(m
          .toIterator
          .map { case (k, v) => (Key(k), v) }
          .filter(x => f(x._1))
          .toMap)
    }
}

object StoreObject extends StoreObject