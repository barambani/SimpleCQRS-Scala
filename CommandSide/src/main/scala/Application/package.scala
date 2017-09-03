package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.Events._

import scalaz.Kleisli

import scala.language.higherKinds

package object Application {
  
  type StoreRetrieve[F[_]] = Kleisli[F, UUID, List[Event]]
  type StoreInsert[F[_]]   = Kleisli[F, List[Event], Unit]
  type CacheGet[F[_], A] = Kleisli[F, UUID, Option[A]]
  type CachePut[F[_], A] = Kleisli[F, A, Unit]
}
