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
	val apply: List[Event] => A
	val newState: A => Event => A
}
object Aggregate {

	implicit lazy val inventoryItemAggregate = new Aggregate[InventoryItem] {
		lazy val newState: InventoryItem => Event => InventoryItem = InventoryItem.newState
		lazy val apply: List[Event] => InventoryItem = InventoryItem.apply
	}

	implicit lazy val orderAggregate = new Aggregate[Order] {
		lazy val newState: Order => Event => Order = Order.newState
		lazy val apply: List[Event] => Order = Order.apply
	}
}

object AggregateRoot {

	import DomainState._

	def evolve[A: Aggregate](aState: A, withHistory: List[Event]): A =
		(withHistory foldRight aState) {
			(e, s) => implicitly[Aggregate[A]].newState(s)(e)
		}

	def rehydrated[A: Aggregate]: List[Event] => A =
		history => implicitly[Aggregate[A]].apply(history)

	def newStateTransition[A: Aggregate](commandExecution: CommandExecution[A]): StateTransition[A] = 
		for {
			es 	<- State.gets(commandExecution)
			_ 	<- State.modify { s: A => evolve(s, es) }
		} yield es
}
