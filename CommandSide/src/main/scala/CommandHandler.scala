package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain._

object CommandHandler {

	import DomainState._
	
	def handleOne(historyRepository: Identified => List[Event])(command: Command): List[Event] =
		tryApplyCommand(command) map (f => (f compose historyRepository)(command)) getOrElse Nil

	def handleMany(historyRepository: Identified => List[Event])(commands: List[Command]): List[Event] =
		commands flatMap handleOne(historyRepository)

	private def tryApplyCommand(command: Command): Option[List[Event] => List[Event]] =
		tryApplyToInventoryItem.lift(command) map (eventEmissionForTransition(_)) orElse (tryApplyToOrder.lift(command) map (eventEmissionForTransition(_)))

	import InventoryItem._
	private def tryApplyToInventoryItem: PartialFunction[Command, EitherTransition[InventoryItem]] = {
		case CreateInventoryItem(id, name) 		=> InventoryItem.createFor(id)(name)
		case DeactivateInventoryItem(_) 		=> deactivateInventoryItem
		case RenameInventoryItem(_, newName) 	=> renameInventoryItem(newName)
		case CheckInItemsToInventory(_, count)	=> checkInItemsToInventory(count)
		case RemoveItemsFromInventory(_, count)	=> removeItemsFromInventory(count)
	}

	import Order._
	private def tryApplyToOrder: PartialFunction[Command, EitherTransition[Order]] = {
		case CreateOrder(id, customerId, customerName) 					=> Order.createFor(id)(s"$customerId - $customerName")
		case AddInventoryItemToOrder(_, inventoryItemId, quantity) 		=> addInventoryItemToOrder(inventoryItemId)(quantity)
		case RemoveInventoryItemFromOrder(_, inventoryItemId, quantity)	=> removeInventoryItemFromOrder(inventoryItemId)(quantity)
		case AddShippingAddressToOrder(_, shippingAddress)				=> addShippingAddressToOrder(shippingAddress)
		case PayForTheOrder(_) 											=> payTheBalance
		case SubmitTheOrder(_)											=> submit
	}

	private def eventEmissionForTransition[A : Aggregate](stateTransition: StateTransition[A]): List[Event] => List[Event] =
		evalTransition(stateTransition) compose AggregateRoot.rehydrated[A] _
}


