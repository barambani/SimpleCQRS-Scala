package SimpleCqrsScala.CommandSide.CommandHandler

import java.util.UUID

import SimpleCqrsScala.CommandSide.Commands._
import SimpleCqrsScala.CommandSide.Events._
import SimpleCqrsScala.CommandSide.InventoryItem._
import SimpleCqrsScala.CommandSide.EventStore._

object CommandHandler {
	def apply(eventStore: Repository) = new CommandHandler(eventStore)
} 

class CommandHandler(eventStore: Repository) {

	def handle(command: Command): Unit = {

		def readHistory(id: UUID): List[Event] = eventStore GetHistoryById id

		command match {

			case CreateInventoryItem(id, name) => 
				eventStore Save InventoryItemCreated(id, name, 1)
			
			case DeactivateInventoryItem(id) => {
				val event = InventoryItem(readHistory(id)).deactivateInventoryItem 
				eventStore Save event
			}
			case RenameInventoryItem(id, newName) => {
				val item = InventoryItem(readHistory(id))
				val event = item renameInventoryItem newName
				eventStore Save event
			}
			case CheckInItemsToInventory(id, count) => {
				val item = InventoryItem(readHistory(id))
				val event = item checkInItemsToInventory count
				eventStore Save event
			}
			case RemoveItemsFromInventory(id, count) => {
				val item = InventoryItem(readHistory(id))
				val event = item removeItemsFromInventory count
				eventStore Save event
			}
			case _ => 
		}

	}
}