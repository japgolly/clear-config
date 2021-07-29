package japgolly.clearconfig.internals

import cats.Applicative

object StoreJvm extends StoreObjectJvm
trait StoreObjectJvm extends StoreObject {

  final override def environment[F[_]](implicit F: Applicative[F]): Store[F] =
    StoreJvm.ofMap(sys.env)

}
