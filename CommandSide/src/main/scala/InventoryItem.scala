package SimpleCqrsScala.CommandSide.InventoryItem

import SimpleCqrsScala.CommandSide.Events._
import java.util.UUID

object InventoryItem {

	private val initialState = new InventoryItem()

	def apply(historyStep: Event): InventoryItem = 
		apply(List(historyStep))
	
	def apply(history: List[Event]): InventoryItem =
		(history foldRight initialState) ((e, s) => s getStateWhen e)
}

case class InventoryItem private (
	id: UUID, 
	name: String, 
	isActivated: Boolean,
	version: Long) {
	
	private def this() = this(new UUID(0, 0), "", false, 0)

	private def nextVersion: Long = version + 1
	private def isInSequence(event: Sequenced): Boolean = event.sequence == nextVersion

	private def isValid(name: String): Boolean = false

	def getStateWhen(event: Event): InventoryItem = 
		if(!isInSequence(event)) this // TODO: Error in this case
		else event match {
			case InventoryItemCreated(newId, newName, sequence) => new InventoryItem(newId, newName, true, sequence)
			case InventoryItemDeactivated(toDeactivateId, sequence) => 
				if(toDeactivateId == id) new InventoryItem(id, name, false, sequence)
				else this
			case _ => this
		}

	def create(id: UUID, name: String): Event = InventoryItemCreated(id, name, nextVersion)
	def deactivateInventoryItem: Event = InventoryItemDeactivated(id, nextVersion)
	def renameInventoryItem(newName: String): Event = InventoryItemRenamed(id, newName, nextVersion)
	def checkInItemsToInventory(count: Int): Event = ItemsCheckedInToInventory(id, count, nextVersion)
	def removeItemsFromInventory(count: Int): Event = ItemsRemovedFromInventory(id, count, nextVersion)
}