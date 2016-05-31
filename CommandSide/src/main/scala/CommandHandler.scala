package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain._

object CommandHandler {
	
	def handle(retrieveHistory: Identified => List[Event])(command: Command): List[Event] =
		applyCommand(command) map (f => (f compose retrieveHistory)(command)) getOrElse Nil

	private def applyCommand(command: Command): Option[List[Event] => List[Event]] =
		(ApplyCommandToInventoryItem lift command) orElse (ApplyCommandToOrder lift command)

	private def ApplyCommandToInventoryItem: PartialFunction[Command, List[Event] => List[Event]] = {
		case CreateInventoryItem(id, name) 		=> Nil => InventoryItemCreated(id, name, 1) asHistory
		case DeactivateInventoryItem(_) 		=> nextEvolutionFor[InventoryItem](i => i deactivateInventoryItem)
		case RenameInventoryItem(_, newName) 	=> nextEvolutionFor[InventoryItem](i => i renameInventoryItem newName)
		case CheckInItemsToInventory(_, count)	=> nextEvolutionFor[InventoryItem](i => i checkInItemsToInventory count)
		case RemoveItemsFromInventory(_, count)	=> nextEvolutionFor[InventoryItem](i => i removeItemsFromInventory count)
	}

	private def ApplyCommandToOrder: PartialFunction[Command, List[Event] => List[Event]] = {
		case CreateOrder(id, customerId, customerName) => Nil => NewOrderCreated(id, s"$customerId - $customerName", 1) asHistory
	}

	private def nextEvolutionFor[A: Aggregate](behavior: A => List[Event]): List[Event] => List[Event] = {
		behavior compose AggregateRoot.createFrom[A] _
	}
}