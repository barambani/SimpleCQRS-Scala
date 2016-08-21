package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._

import scalaz._

trait Versioned {
	val version: Long
}

trait Identity {
	val id: UUID
}

trait Aggregate[A] {
	def apply(history: List[Event]): A
	def getNewStateFor(currentState: A, event: Event): A
}
object Aggregate {

	import InventoryItemOps._
	import OrderOps._
	
	implicit lazy val inventoryItemAggregate = new Aggregate[InventoryItem] {
		def getNewStateFor(currentState: InventoryItem, event: Event): InventoryItem = InventoryItemOps.eventReceivedAtState(event)(currentState)
		def apply(history: List[Event]): InventoryItem = InventoryItem(history)
	}

	implicit lazy val orderAggregate = new Aggregate[Order] { 
		def getNewStateFor(currentState: Order, event: Event): Order = OrderOps.eventReceivedAtState(event)(currentState)
		def apply(history: List[Event]): Order = Order(history)
	}
}

object AggregateRoot {

	import DomainStates._

	def evolve[A : Aggregate](aState: A, history: List[Event]): A = 
		(history foldRight aState) ((e, s) => implicitly[Aggregate[A]].getNewStateFor(s, e))

	def createFrom[A: Aggregate](history: List[Event]): A = implicitly[Aggregate[A]].apply(history)

	def getNewState[A : Aggregate](esg: A => List[Event]): EvolvableState[A] = for {
		es 	<- State.gets(esg)
		_ 	<- State.modify { s: A => evolve(s, es) }
	} yield es
}