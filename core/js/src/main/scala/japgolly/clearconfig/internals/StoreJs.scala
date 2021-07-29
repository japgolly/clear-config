package japgolly.clearconfig.internals

import cats.Applicative
import scala.scalajs.js

object StoreJs extends StoreObjectJs
trait StoreObjectJs extends StoreObject {

  final override def environment[F[_]](implicit F: Applicative[F]): Store[F] = {

    // Try for Node's process.env
    try {
      js.Dynamic.global.process.env match {
        case o: js.Object => return StoreJs.ofJsObject(o)
        case _ =>
      }
    } catch {
      case _: Throwable =>
    }

    // Give up
    StoreJs.empty[F]
  }

  final def ofJsObject[F[_]](o: js.Object)(implicit F: Applicative[F]): Store[F] =
    apply(F.point {
      o.asInstanceOf[js.Dictionary[Any]]
      .iterator
      .map { case (k, v) => Key(k) -> ("" + v) }
      .toMap
    })

}
