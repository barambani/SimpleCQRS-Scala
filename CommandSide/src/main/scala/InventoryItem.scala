package SimpleCqrsScala.CommandSide.InventoryItem

import java.util.UUID

import SimpleCqrsScala.CommandSide.Events._

object InventoryItem {

	private val initialId = new UUID(0, 0)
	private val initialState = new InventoryItem()

	def apply(historyStep: Event): InventoryItem = 
		apply(List(historyStep))
	
	def apply(history: List[Event]): InventoryItem =
		(history foldRight initialState) ((e, s) => s getStateWhen e)
}

case class InventoryItem private (
	id: UUID, 
	name: String, 
	isActivated: Boolean ,
	itemsCount: Int = 0,
	version: Long = 0) {
	
	private def this() = this(InventoryItem.initialId, "", false)

	private def hasTheCorrectId(event: Identity): Boolean = 
		id == new UUID(0, 0) || event.id == id

	private def nextVersion: Long = version + 1
	private def isInSequence(event: Sequenced): Boolean = event.sequence == nextVersion

	private def countAfterCheckIn(toCheckin: Int): Int = itemsCount + toCheckin
	private def countAfterRemoval(toRemove: Int): Int = itemsCount - toRemove

	def getStateWhen(event: Event): InventoryItem = 

		if(!isInSequence(event)) this // TODO: Error in this case
		else if(!hasTheCorrectId(event)) this // TODO: Error in this case
		else event match {
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
	def create(id: UUID, name: String): Event = {
		//if(id == InventoryItem.initialId) ERROR
		//if(newName,ieEmpty) ERROR
		InventoryItemCreated(id, name, nextVersion)
	}

	def deactivateInventoryItem: Event =
		InventoryItemDeactivated(id, nextVersion)

	def renameInventoryItem(newName: String): Event =
		//if(newName,ieEmpty) ERROR
		InventoryItemRenamed(id, newName, nextVersion)

	def checkInItemsToInventory(count: Int): Event =
		ItemsCheckedInToInventory(id, count, nextVersion)

	def removeItemsFromInventory(count: Int): Event =
		//if(itemsCount - count < 0) ERROR
		ItemsRemovedFromInventory(id, count, nextVersion)
}