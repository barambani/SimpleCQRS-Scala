package SimpleCqrsScala.CommandSide.Application

import SimpleCqrsScala.CommandSide.Domain._

import scalaz.Kleisli
import scalaz.Monad
import scala.language.higherKinds

object CacheType {
 
  type LocalActor = LocalActor.type
  type Memcached = Memcached.type

  sealed trait CacheType
  final case object LocalActor extends CacheType
  final case object Memcached extends CacheType
}

trait Cache[C, A] {
  def read[F[_]](implicit MO: Monad[F]): CacheGet[F, A]
  def write[F[_]](implicit MO: Monad[F]): CachePut[F, A]
}

import SimpleCqrsScala.CommandSide.Application.CacheType._

object Cache {
  def apply[C <: CacheType, A](implicit INST: Cache[C, A], AG: Aggregate[A]): Cache[C, A] = INST
}

//  ----------------------------
//  Actual cache implementations
//  ----------------------------

trait LocalActorCache {

  implicit val orderActorCache = new Cache[LocalActor, Order] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, Order] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, Order] =
      Kleisli { a => MO.point { () } }
  }

  implicit val inventoryItemActorCache = new Cache[LocalActor, InventoryItem] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, InventoryItem] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, InventoryItem] =
      Kleisli { a => MO.point { () } }
  }
}

trait MemcachedCache{

  implicit val orderActorCache = new Cache[Memcached, Order] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, Order] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, Order] =
      Kleisli { a => MO.point { () } }
  }

  implicit val inventoryItemActorCache = new Cache[Memcached, InventoryItem] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, InventoryItem] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, InventoryItem] =
      Kleisli { a => MO.point { () } }
  }
}
