package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._

import scalaz._

trait Versioned {
	val version: Long
	val expectedNextVersion: Long = version + 1
}

trait Identity {
	val id: UUID
}

trait Aggregate[A] {
	val newState: A => Event => A
	val rehydrate: List[Event] => A
}
object Aggregate {

	implicit object InventoryItemAggregate extends Aggregate[InventoryItem] {
		lazy val newState: InventoryItem => Event => InventoryItem = InventoryItem.newState
		lazy val rehydrate: List[Event] => InventoryItem = InventoryItem.rehydrate
	}

	implicit object OrderAggregate extends Aggregate[Order] {
		lazy val newState: Order => Event => Order = Order.newState
		lazy val rehydrate: List[Event] => Order = Order.rehydrate
	}
}

object AggregateRoot {

	import DomainState._

	def evolve[A: Aggregate](aState: A)(withHistory: List[Event])(implicit AGG: Aggregate[A]): A =
		withHistory.foldRight(aState){ (e, s) => AGG.newState(s)(e) }

	def rehydrated[A: Aggregate](history: List[Event])(implicit AGG: Aggregate[A]): A = AGG.rehydrate(history)

	def newStateTransition[A: Aggregate](commandExecution: CommandExecution[A]): StateTransition[A] = 
		for {
			es 	<- State.gets(commandExecution)
			_ 	<- State.modify { s: A => evolve(s)(es) }
		} yield es
}
