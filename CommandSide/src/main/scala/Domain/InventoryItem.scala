package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._

object InventoryItem {
	def apply(history: List[Event]): InventoryItem = AggregateRoot.evolve(new InventoryItem, history)
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

	//	Behavior
	def deactivateInventoryItem: List[Event] =
		InventoryItemDeactivated(id, nextStateVersion).asHistory

	def renameInventoryItem(newName: String): List[Event] =
		if(theNameIsValid(newName)) InventoryItemRenamed(id, newName, nextStateVersion).asHistory
		else Nil // TODO: Error, the new name is not valid

	def checkInItemsToInventory(count: Int): List[Event] =
		ItemsCheckedInToInventory(id, count, nextStateVersion).asHistory

	def removeItemsFromInventory(count: Int): List[Event] = 
		if(itemsCanBeRemoved(count)) ItemsRemovedFromInventory(id, count, nextStateVersion).asHistory
		else Nil // TODO: Error, not enough items to remove
		
}