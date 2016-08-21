package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainStates._

import scalaz._

object InventoryItemOps {

	import AggregateRoot._
	import EventOps._

	//	Behavior
	def deactivateInventoryItem: StateTransition[InventoryItem] = 
		newStateTransition(i => InventoryItemDeactivated(i.id, nextStateVersion(i)).asHistory)

	def checkInItemsToInventory(count: Int): StateTransition[InventoryItem] =
		newStateTransition(i => ItemsCheckedInToInventory(i.id, count, nextStateVersion(i)).asHistory)

	def renameInventoryItem(newName: String): StateTransition[InventoryItem] = 
		newStateTransition(
			i => if(i.theNameIsValid(newName)) InventoryItemRenamed(i.id, newName, nextStateVersion(i)).asHistory 
				 else Nil // TODO: Error, the new name is not valid
		)
	
	def removeItemsFromInventory(count: Int): StateTransition[InventoryItem] = 
		newStateTransition(
			i => if(i.itemsCanBeRemoved(count)) ItemsRemovedFromInventory(i.id, count, nextStateVersion(i)).asHistory
				 else Nil // TODO: Error, not enough items to remove
		)

	lazy val newState: InventoryItem => Event => InventoryItem = 
		aggregate => event =>
			if(!hasACorrectIdCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequenceCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else 
				event match {
					case InventoryItemCreated(newId, newName, sequence) => 
						if(aggregate.theNameIsValid(newName)) new InventoryItem(newId, newName, true, version = sequence)
						else aggregate // TODO: Error, the new name is not valid
					
					case InventoryItemDeactivated(_, sequence) => 
						new InventoryItem(aggregate.id, aggregate.name, false, aggregate.itemsCount, sequence)

					case InventoryItemRenamed(_, newName, sequence) => 
						if(aggregate.theNameIsValid(newName)) new InventoryItem(
							aggregate.id, 
							newName, 
							aggregate.isActivated, 
							aggregate.itemsCount, 
							sequence
						)
						else aggregate // TODO: Error, the new name is not valid

					case ItemsCheckedInToInventory(_, count, sequence) => 
						new InventoryItem(aggregate.id, aggregate.name, aggregate.isActivated, aggregate.countAfterCheckIn(count), sequence)
					
					case ItemsRemovedFromInventory(_, count, sequence) => 
						if(aggregate.itemsCanBeRemoved(count)) new InventoryItem(
							aggregate.id, 
							aggregate.name, 
							aggregate.isActivated, 
							aggregate.countAfterRemoval(count), 
							sequence
						)
						else aggregate // TODO: Error, not enough items to remove
					
					case _ => aggregate // TODO: log event ignored with event details
				}

	object InventoryItem {
		import AggregateRoot._

		def apply(history: List[Event]): InventoryItem = evolve(new InventoryItem, history)
	}
	class InventoryItem private[InventoryItemOps] (
		val id: UUID = new UUID(0, 0), 
		val name: String = "", 
		val isActivated: Boolean = false,
		val itemsCount: Int = 0,
		val version: Long = 0) extends Identity with Versioned {
		
		//	Domain logic
		lazy val countAfterCheckIn: Int => Int = toCheckin => itemsCount + toCheckin
		lazy val countAfterRemoval: Int => Int = toRemove => itemsCount - toRemove
		lazy val itemsCanBeRemoved: Int => Boolean = count => itemsCount >= count
		lazy val theNameIsValid: String => Boolean = n => !n.isEmpty
	}
}
