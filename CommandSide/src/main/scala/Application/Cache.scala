package SimpleCqrsScala.CommandSide.Application

import SimpleCqrsScala.CommandSide.Domain._

import java.util.UUID
import scalaz.Kleisli
import cats.effect._

import SimpleCqrsScala.CommandSide.Domain.Events._

object CacheType {
 
  type LocalActor = LocalActor.type
  type Memcached = Memcached.type

  sealed trait CacheType
  final case object LocalActor extends CacheType
  final case object Memcached extends CacheType
}

trait Cache[C, A] {
  def read: CacheGet[A]
  def write: CachePut[A]
}

import SimpleCqrsScala.CommandSide.Application.CacheType._

object Cache {
  def apply[C <: CacheType, A](implicit INST: Cache[C, A], AG: Aggregate[A]): Cache[C, A] = INST
}

trait ActorCache {

  implicit val orderActorCache = new Cache[LocalActor, Order] {

    def read: CacheGet[Order] = 
      Kleisli { id => IO { None } }

    def write: CachePut[Order] =
      Kleisli { a => IO { () } }
  }

  implicit val inventoryItemActorCache = new Cache[LocalActor, InventoryItem] {

    def read: CacheGet[InventoryItem] = 
      Kleisli { id => IO { None } }

    def write: CachePut[InventoryItem] =
      Kleisli { a => IO { () } }
  }
}
