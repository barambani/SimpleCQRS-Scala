package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._

import scalaz._

trait Versioned {
	val version: Long
	protected def nextStateVersion: Long = version + 1
	protected def isInSequence(event: Sequenced): Boolean = event.sequence == nextStateVersion
}

trait Identity {
	val id: UUID
	protected def hasTheCorrectId(event: Identified): Boolean = id == new UUID(0, 0) || event.id == id
}

trait Aggregate[A] {
	def apply(history: List[Event]): A
	def getNewStateFor(currentState: A, happens: Event): A
}

object Aggregate {
	
	implicit lazy val inventoryItemAggregate = new Aggregate[InventoryItem] { 
		def getNewStateFor(currentState: InventoryItem, happens: Event): InventoryItem = currentState getNewStateWhen happens
		def apply(history: List[Event]): InventoryItem = InventoryItem(history)
	}

	implicit lazy val orderAggregate = new Aggregate[Order] { 
		def getNewStateFor(currentState: Order, happens: Event): Order = currentState getNewStateWhen happens
		def apply(history: List[Event]): Order = Order(history)
	}
}

object AggregateRoot {

	import DomainTypes._
	
	def evolve[A : Aggregate](initialState: A, history: List[Event]): A = 
		(history foldRight initialState) ((e, s) => implicitly[Aggregate[A]].getNewStateFor(s, e))

	def createFrom[A: Aggregate](history: List[Event]): A = implicitly[Aggregate[A]].apply(history)

	def getNewState[A : Aggregate](esg: A => List[Event]): EvolvableState[A] = for {
		es 	<- State.gets(esg)
		_ 	<- State.modify { s: A => AggregateRoot.evolve(s, es) }
	} yield es
}