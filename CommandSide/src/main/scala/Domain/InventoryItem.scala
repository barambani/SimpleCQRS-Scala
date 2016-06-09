package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainTypes._

import scalaz._

object InventoryItem {

	import AggregateRoot._

	def apply(history: List[Event]): InventoryItem = evolve(new InventoryItem, history)

	//	Behavior
	def deactivateInventoryItem: InventoryItemS = 
		getState(i => InventoryItemDeactivated(i.id, i.nextStateVersion).asHistory)

	def checkInItemsToInventory(count: Int): InventoryItemS =
		getState(i => ItemsCheckedInToInventory(i.id, count, i.nextStateVersion).asHistory)

	def renameInventoryItem(newName: String): InventoryItemS = getState(
		i => if(i.theNameIsValid(newName)) InventoryItemRenamed(i.id, newName, i.nextStateVersion).asHistory 
			else Nil // TODO: Error, the new name is not valid
	)
	
	def removeItemsFromInventory(count: Int): InventoryItemS = getState(
		i => if(i.itemsCanBeRemoved(count)) ItemsRemovedFromInventory(i.id, count, i.nextStateVersion).asHistory
			else Nil // TODO: Error, not enough items to remove
	)
}

class InventoryItem private (
	val id: UUID = new UUID(0, 0), 
	val name: String = "", 
	val isActivated: Boolean = false,
	val itemsCount: Int = 0,
	val version: Long = 0) extends Identity with Versioned {
	
	private def countAfterCheckIn(toCheckin: Int): Int = itemsCount + toCheckin
	private def countAfterRemoval(toRemove: Int): Int = itemsCount - toRemove

	//	Domain logic
	private def itemsCanBeRemoved(count: Int): Boolean = itemsCount >= count
	private def theNameIsValid(n: String): Boolean = !n.isEmpty

	def getNewStateWhen(event: Event): InventoryItem = 
		if(!hasTheCorrectId(event)) this // TODO: Error in this case
		else if(!isInSequence(event)) this // TODO: Error in this case
		else 
			event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					if(theNameIsValid(newName)) new InventoryItem(newId, newName, true, version = sequence)
					else this // TODO: Error, the new name is not valid
				
				case InventoryItemDeactivated(_, sequence) => 
					new InventoryItem(id, name, false, itemsCount, sequence)

				case InventoryItemRenamed(_, newName, sequence) => 
					if(theNameIsValid(newName)) new InventoryItem(id, newName, isActivated, itemsCount, sequence)
					else this // TODO: Error, the new name is not valid

				case ItemsCheckedInToInventory(_, count, sequence) => 
					new InventoryItem(id, name, isActivated, countAfterCheckIn(count), sequence)
				
				case ItemsRemovedFromInventory(_, count, sequence) => 
					if(itemsCanBeRemoved(count)) new InventoryItem(id, name, isActivated, countAfterRemoval(count), sequence)
					else this // TODO: Error, not enough items to remove
				
				case _ => this
			}
}