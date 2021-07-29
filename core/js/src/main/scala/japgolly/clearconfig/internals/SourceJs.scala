package japgolly.clearconfig.internals

import cats.Applicative
import scala.scalajs.js

object SourceNameJs extends SourceNameObjectJs
trait SourceNameObjectJs extends SourceNameObject {
}

object SourceJs extends SourceObjectJs
trait SourceObjectJs extends SourceObject {

  override protected def envStore[F[_]](implicit F: Applicative[F]): Store[F] = {

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

}
