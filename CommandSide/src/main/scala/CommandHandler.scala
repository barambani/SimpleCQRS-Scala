package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain._

object CommandHandler {

	import DomainState._
	
	def handle(retrieveHistory: Identified => List[Event])(command: Command): List[Event] =
		applyCommand(command) map (f => (f compose retrieveHistory)(command)) getOrElse Nil

	lazy private val applyCommand: Command => Option[List[Event] => List[Event]] =
		command => ApplyCommandToInventoryItem.lift(command) orElse ApplyCommandToOrder.lift(command)

	import InventoryItemOps._
	private def ApplyCommandToInventoryItem: PartialFunction[Command, List[Event] => List[Event]] = {
		case CreateInventoryItem(id, name) 		=> Nil => InventoryItemCreated(id, name, 1) :: Nil
		case DeactivateInventoryItem(_) 		=> eventEmissionForTransition(deactivateInventoryItem)
		case RenameInventoryItem(_, newName) 	=> eventEmissionForTransition(renameInventoryItem(newName))
		case CheckInItemsToInventory(_, count)	=> eventEmissionForTransition(checkInItemsToInventory(count))
		case RemoveItemsFromInventory(_, count)	=> eventEmissionForTransition(removeItemsFromInventory(count))
	}

	import OrderOps._
	private def ApplyCommandToOrder: PartialFunction[Command, List[Event] => List[Event]] = {
		case CreateOrder(id, customerId, customerName) 					=> Nil => OrderCreated(id, s"$customerId - $customerName", 1) :: Nil
		case AddInventoryItemToOrder(_, inventoryItemId, quantity) 		=> eventEmissionForTransition(addInventoryItemToOrder(inventoryItemId, quantity))	
		case RemoveInventoryItemFromOrder(_, inventoryItemId, quantity)	=> eventEmissionForTransition(removeInventoryItemFromOrder(inventoryItemId, quantity))
		case AddShippingAddressToOrder(_, shippingAddress)				=> eventEmissionForTransition(addShippingAddressToOrder(shippingAddress))
		case PayForTheOrder(_) 											=> eventEmissionForTransition(payTheBalance)
		case SubmitTheOrder(_)											=> eventEmissionForTransition(submit)
	}

	private def eventEmissionForTransition[A](stateTransition: StateTransition[A])(implicit a: Aggregate[A]): List[Event] => List[Event] =
		stateTransition.eval _ compose AggregateRoot.rehydrated[A] _
}
