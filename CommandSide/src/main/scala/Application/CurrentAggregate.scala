package SimpleCqrsScala.CommandSide.Application

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, DomainAggregates, Identity, Order, InventoryItem}
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

import scalaz.Kleisli
import cats.effect._

trait CurrentAggregateState[CT, ST, A] {

	implicit val CA: Cache[CT, A]
	implicit val ES: EventStore[ST]
	implicit val AGG: Aggregate[A]
	
	def fromCacheOrRehydrate: Kleisli[IO, UUID, A] =
		CA.read flatMap { 
			mayBeAgg => mayBeAgg.fold(rehydrateFromStore){ 
				agg => Kleisli{ _ => IO { agg } }
			}
		}

	private def rehydrateFromStore: Kleisli[IO, UUID, A] =
		ES.read map { events => AGG.rehydrate(events) }
}
object CurrentAggregateState {
	def apply[CT <: CacheType, ST <: EventStoreType, A](implicit INST: CurrentAggregateState[CT, ST, A]): CurrentAggregateState[CT, ST, A] = INST
}

trait CurrentAggregateStateImpl extends EventStoreTypes with ActorCache {

	implicit val currentOrderAggregate1 = new CurrentAggregateState[LocalActor, Cassandra, Order] {
		val CA = Cache[LocalActor, Order]
		val ES = EventStore[Cassandra]
		val AGG = Aggregate[Order]
	}

	implicit val currentInventoryItemAggregate1 = new CurrentAggregateState[LocalActor, Cassandra, InventoryItem] {
		val CA = Cache[LocalActor, InventoryItem]
		val ES = EventStore[Cassandra]
		val AGG = Aggregate[InventoryItem]
	}
}