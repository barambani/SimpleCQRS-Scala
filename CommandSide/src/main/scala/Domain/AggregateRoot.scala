package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import scala.annotation._

trait Versioned {
	val version: Long
	val expectedNextVersion: Long = version + 1
}

trait Identity {
	val id: UUID
}

trait Aggregate[A] {
	val zero: A
	val newState: A => Event => A
	val rehydrate: List[Event] => A
}

object Aggregate {
	
	def apply[A](implicit instance: Aggregate[A]): Aggregate[A] = instance

	implicit object InventoryItemAggregate extends Aggregate[InventoryItem] {
		lazy val zero: InventoryItem = InventoryItem.empty
		lazy val newState: InventoryItem => Event => InventoryItem = InventoryItem.newState
		lazy val rehydrate: List[Event] => InventoryItem = InventoryItem.rehydrate
	}

	implicit object OrderAggregate extends Aggregate[Order] {
		lazy val zero: Order = Order.empty
		lazy val newState: Order => Event => Order = Order.newState
		lazy val rehydrate: List[Event] => Order = Order.rehydrate
	}
}

object AggregateLaws {
	def law1[A](implicit AGG: Aggregate[A]): Boolean = AGG.rehydrate(Nil) == AGG.zero
	def law2[A](implicit AGG: Aggregate[A]): Boolean = AGG.rehydrate(Event.zeroEvent :: Nil) == AGG.zero
	def law3[A](implicit AGG: Aggregate[A]): Boolean = AGG.newState(AGG.zero)(Event.zeroEvent) == AGG.zero
}

object AggregateRoot {

	import DomainState._

	@implicitNotFound("implicit not found for Aggregate[{A}]")
	def evolve[A](aState: A)(withHistory: List[Event])(implicit AGG: Aggregate[A]): A =
		withHistory.foldRight(aState){ (e, s) => AGG.newState(s)(e) }

	@implicitNotFound("implicit not found for Aggregate[{A}]")
	def rehydrated[A](history: List[Event])(implicit AGG: Aggregate[A]): A = 
		AGG.rehydrate(history)
}