package SimpleCqrsScala.CommandSide.Application

import SimpleCqrsScala.CommandSide.Domain._

import java.util.UUID
import scalaz.Kleisli
import cats.effect._

import SimpleCqrsScala.CommandSide.Domain.Events._

sealed trait CacheType

sealed trait LocalActor extends CacheType
object LocalActor extends LocalActor

sealed trait Memcached extends CacheType
object Memcached extends Memcached

trait Cache[C, A] {
  def read: CacheGet[A]
  def write: CachePut[A]
}

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
