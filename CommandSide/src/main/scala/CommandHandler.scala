package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain._

object CommandHandler {
	def apply(eventStore: Repository) = new CommandHandler(eventStore)
}

class CommandHandler(eventStore: Repository) {

	def handle(command: Command): Unit = {

		command match {
			case CreateInventoryItem(id, name) => 
				eventStore Save InventoryItemCreated(id, name, 1).asHistory

			case DeactivateInventoryItem(id) => 
				eventStore Save invokeBehaviorOn(id, i => i deactivateInventoryItem)

			case RenameInventoryItem(id, newName) => 
				eventStore Save invokeBehaviorOn(id, i => i renameInventoryItem newName)

			case CheckInItemsToInventory(id, count) => 
				eventStore Save invokeBehaviorOn(id, i => i checkInItemsToInventory count)

			case RemoveItemsFromInventory(id, count) => 
				eventStore Save invokeBehaviorOn(id, i => i removeItemsFromInventory count)

			case _ => 
		}

		def invokeBehaviorOn(id: UUID, behavior: InventoryItem => List[Event]): List[Event] = {
			def readHistory(id: UUID): List[Event] = eventStore GetHistoryById id
			def applyBehaviorTo: UUID => List[Event] = behavior compose InventoryItem.apply _ compose readHistory _

			applyBehaviorTo(id)
		}
	}
}