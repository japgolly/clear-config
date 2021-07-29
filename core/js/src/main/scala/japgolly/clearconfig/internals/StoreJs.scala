package japgolly.clearconfig.internals

import cats.Applicative
import scala.scalajs.js

object StoreJs extends StoreObjectJs
trait StoreObjectJs extends StoreObject {

  final def ofJsObject[F[_]](o: js.Object)(implicit F: Applicative[F]): Store[F] =
    apply(F.point {
      o.asInstanceOf[js.Dictionary[Any]]
      .iterator
      .map { case (k, v) => Key(k) -> ("" + v) }
      .toMap
    })

}
