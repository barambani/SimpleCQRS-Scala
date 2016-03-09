package InventoryItem

import Events._
import java.util.UUID

object InventoryItem {

	val initialState = new InventoryItem()

	def evolve(initialState: InventoryItem, historyStep: Event): InventoryItem = 
		evolve(initialState, List(historyStep))
	
	def evolve(initialState: InventoryItem, history: List[Event]): InventoryItem =
		(history foldLeft initialState)((s, e) => s stateAfterTheEvent e)
}

case class InventoryItem private (id: UUID, name: String, isActivated: Boolean) {
	
	private def this() = this(new UUID(0, 0), "", false)

	def stateAfterTheEvent(event: Event): InventoryItem = event match {

		case InventoryItemCreated(newId, newName) => new InventoryItem(newId, newName, true)
		case InventoryItemDeactivated(toDeactivateId) => 
			if(toDeactivateId == id) new InventoryItem(id, name, false)
			else this
		case _ => this
	}
}