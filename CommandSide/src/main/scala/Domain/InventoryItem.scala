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

	def getNewStateWhen(event: Event): InventoryItem = 

		if(!isInSequence(event)) this // TODO: Error in this case
		else if(!hasTheCorrectId(event)) this // TODO: Error in this case
		else 
			event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					new InventoryItem(newId, newName, true, version = sequence)
				
				case InventoryItemDeactivated(toDeactivateId, sequence) => 
					new InventoryItem(id, name, false, version = sequence)

				case InventoryItemRenamed(toRenameId, newName, sequence) => 
					new InventoryItem(id, newName, isActivated, itemsCount, sequence)

				case ItemsCheckedInToInventory(toCheckinid, count, sequence) => 
					new InventoryItem(id, name, isActivated, countAfterCheckIn(count), sequence)
				
				case ItemsRemovedFromInventory(toRemoveId, count, sequence) => 
					new InventoryItem(id, name, isActivated, countAfterRemoval(count), sequence)
				
				case _ => this
			}

	//	Behavior
	def deactivateInventoryItem: List[Event] =
		InventoryItemDeactivated(id, nextStateVersion) asHistory

	def renameInventoryItem(newName: String): List[Event] =
		//if(newName,ieEmpty) ERROR
		InventoryItemRenamed(id, newName, nextStateVersion) asHistory

	def checkInItemsToInventory(count: Int): List[Event] =
		ItemsCheckedInToInventory(id, count, nextStateVersion) asHistory

	def removeItemsFromInventory(count: Int): List[Event] = 
		//if(itemsCount - count < 0) ERROR
		ItemsRemovedFromInventory(id, count, nextStateVersion) asHistory
}
