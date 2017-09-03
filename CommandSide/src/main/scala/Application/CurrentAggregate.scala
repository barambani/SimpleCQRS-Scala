package SimpleCqrsScala.CommandSide.Application

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, InventoryItem}
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._

import scalaz.Monad
import scalaz.Kleisli
import scala.language.higherKinds

trait CurrentAggregateState[CT, ST, A] {

  implicit val CA: Cache[CT, A]
  implicit val ES: EventStore[ST]
  implicit val AGG: Aggregate[A]

  def fromCacheOrRehydrate[F[_]](implicit MO: Monad[F]): Kleisli[F, UUID, A] =
    CA.read flatMap { 
      mayBeAgg => mayBeAgg.fold(rehydrateFromStore){ 
        agg => Kleisli { _ => MO.point { agg } }
      }
    }

  private def rehydrateFromStore[F[_]](implicit MO: Monad[F]): Kleisli[F, UUID, A] =
    ES.read map { events => AGG.rehydrate(events) }
}

object CurrentAggregateState {
  def apply[CT <: CacheType, ST <: EventStoreType, A](implicit INST: CurrentAggregateState[CT, ST, A]): CurrentAggregateState[CT, ST, A] = INST
}

//  --------------------------------------
//  Actual aggregate state implementations
//  --------------------------------------

trait CassandraWithLocalCache extends CassandraStoreTypes with LocalActorCache {

  implicit val currentOrderState1 = new CurrentAggregateState[LocalActor, Cassandra, Order] {
    val CA = Cache[LocalActor, Order]
    val ES = EventStore[Cassandra]
    val AGG = Aggregate[Order]
  }

  implicit val currentInventoryItemState1 = new CurrentAggregateState[LocalActor, Cassandra, InventoryItem] {
    val CA = Cache[LocalActor, InventoryItem]
    val ES = EventStore[Cassandra]
    val AGG = Aggregate[InventoryItem]
  }
}

trait CassandraWithMemcached extends CassandraStoreTypes with MemcachedCache {

  implicit val currentOrderState1 = new CurrentAggregateState[Memcached, Cassandra, Order] {
    val CA = Cache[Memcached, Order]
    val ES = EventStore[Cassandra]
    val AGG = Aggregate[Order]
  }

  implicit val currentInventoryItemState1 = new CurrentAggregateState[Memcached, Cassandra, InventoryItem] {
    val CA = Cache[Memcached, InventoryItem]
    val ES = EventStore[Cassandra]
    val AGG = Aggregate[InventoryItem]
  }
}
