package InventoryItem

import Events._
import java.util.UUID

object InventoryItem {

	val initialState = new InventoryItem()

	def evolve(initialState: InventoryItem, historyStep: Event): InventoryItem = 
		evolve(initialState, List(historyStep))
	
	def evolve(initialState: InventoryItem, history: List[Event]): InventoryItem =
		(history foldRight initialState) ((e, s) => s stateAfterTheEvent e)
}

case class InventoryItem private (
	id: UUID, 
	name: String, 
	isActivated: Boolean,
	version: Long) {
	
	private def this() = this(new UUID(0, 0), "", false, 0)

	private def nextVersion: Long = version + 1
	private def isInSequence(event: Sequenced): Boolean = event.sequence == nextVersion

	def stateAfterTheEvent(event: Event): InventoryItem = 
		if(!isInSequence(event)) this
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

	private def isValid(name: String): Boolean = false
}