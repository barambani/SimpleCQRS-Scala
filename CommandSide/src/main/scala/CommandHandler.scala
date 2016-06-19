package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain._

object CommandHandler {

	import DomainStates._
	import InventoryItem._
	import Order._
	
	def handle(retrieveHistory: Identified => List[Event])(command: Command): List[Event] =
		applyCommand(command) map (f => (f compose retrieveHistory)(command)) getOrElse Nil

	private def applyCommand(command: Command): Option[List[Event] => List[Event]] =
		(ApplyCommandToInventoryItem lift command) orElse (ApplyCommandToOrder lift command)

	private def ApplyCommandToInventoryItem: PartialFunction[Command, List[Event] => List[Event]] = {
		case CreateInventoryItem(id, name) 		=> Nil => InventoryItemCreated(id, name, 1).asHistory
		case DeactivateInventoryItem(_) 		=> nextEvolutionFor(deactivateInventoryItem)
		case RenameInventoryItem(_, newName) 	=> nextEvolutionFor(renameInventoryItem(newName))
		case CheckInItemsToInventory(_, count)	=> nextEvolutionFor(checkInItemsToInventory(count))
		case RemoveItemsFromInventory(_, count)	=> nextEvolutionFor(removeItemsFromInventory(count))
	}

	private def ApplyCommandToOrder: PartialFunction[Command, List[Event] => List[Event]] = {
		case CreateOrder(id, customerId, customerName) 					=> Nil => OrderCreated(id, s"$customerId - $customerName", 1).asHistory
		case AddInventoryItemToOrder(_, inventoryItemId, quantity) 		=> nextEvolutionFor(addInventoryItemToOrder(inventoryItemId, quantity))	
		case RemoveInventoryItemFromOrder(_, inventoryItemId, quantity)	=> nextEvolutionFor(removeInventoryItemFromOrder(inventoryItemId, quantity))
		case AddShippingAddressToOrder(_, shippingAddress)				=> nextEvolutionFor(addShippingAddressToOrder(shippingAddress))
		case PayForTheOrder(_) 											=> nextEvolutionFor(payTheBalance)
		case SubmitTheOrder(_)											=> nextEvolutionFor(submit)
	}

	private def nextEvolutionFor[A: Aggregate](behavior: EvolvableState[A]): List[Event] => List[Event] = {
		behavior.eval _ compose AggregateRoot.createFrom[A] _
	}
}