package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain._

import scalaz.\/

object CommandHandler {

	import Repository._

	def handle[C](c: C)(implicit H: Handler[C], ES: EventStore): \/[ErrorMessage, List[Event]] = H.handle(c)

	sealed trait Handler[C] {
		def handle(c: C)(implicit ES: EventStore): \/[ErrorMessage, List[Event]]
	}

	object Handler {

		implicit object CreateInventoryItemH extends Handler[CreateInventoryItem] {
			def handle(c: CreateInventoryItem)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object DeactivateInventoryItemH extends Handler[DeactivateInventoryItem] {
			def handle(c: DeactivateInventoryItem)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object RenameInventoryItemH extends Handler[RenameInventoryItem] {
			def handle(c: RenameInventoryItem)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object CheckInItemsToInventoryH extends Handler[CheckInItemsToInventory] {
			def handle(c: CheckInItemsToInventory)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object RemoveItemsFromInventoryH extends Handler[RemoveItemsFromInventory] {
			def handle(c: RemoveItemsFromInventory)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}


		implicit object CreateOrderH extends Handler[CreateOrder] {
			def handle(c: CreateOrder)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object AddInventoryItemToOrderH extends Handler[AddInventoryItemToOrder] {
			def handle(c: AddInventoryItemToOrder)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object RemoveInventoryItemFromOrderH extends Handler[RemoveInventoryItemFromOrder] {
			def handle(c: RemoveInventoryItemFromOrder)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object AddShippingAddressToOrderH extends Handler[AddShippingAddressToOrder] {
			def handle(c: AddShippingAddressToOrder)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object PayForTheOrderH extends Handler[PayForTheOrder] {
			def handle(c: PayForTheOrder)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
		implicit object SubmitTheOrderH extends Handler[SubmitTheOrder] {
			def handle(c: SubmitTheOrder)(implicit ES: EventStore): \/[ErrorMessage, List[Event]] = ???
		}
	}
}

// object CommandHandler {

// 	import DomainState._
	
// 	def handleOne(historyRepository: Identified => List[Event])(command: Command): List[Event] =
// 		tryApplyCommand(command) map (f => (f compose historyRepository)(command)) getOrElse Nil

// 	def handleMany(historyRepository: Identified => List[Event])(commands: List[Command]): List[Event] =
// 		commands flatMap handleOne(historyRepository)

// 	private def tryApplyCommand(command: Command): Option[List[Event] => List[Event]] =
// 		tryApplyToInventoryItem.lift(command) map (eventEmissionForTransition(_)) orElse (tryApplyToOrder.lift(command) map (eventEmissionForTransition(_)))

	// import InventoryItem._
	// private def tryApplyToInventoryItem: PartialFunction[Command, EitherTransition[InventoryItem]] = {
		//case CreateInventoryItem(id, name) 		=> InventoryItem.createFor(id)(name)
		//case DeactivateInventoryItem(_) 		=> deactivateInventoryItem
		//case RenameInventoryItem(_, newName) 	=> renameInventoryItem(newName)
		//case CheckInItemsToInventory(_, count)	=> checkInItemsToInventory(count)
		//case RemoveItemsFromInventory(_, count)	=> removeItemsFromInventory(count)
	// }

	// import Order._
	//private def tryApplyToOrder: PartialFunction[Command, EitherTransition[Order]] = {
		//case CreateOrder(id, customerId, customerName) 					=> Order.createFor(id)(s"$customerId - $customerName")
		//case AddInventoryItemToOrder(_, inventoryItemId, quantity) 		=> addInventoryItemToOrder(inventoryItemId)(quantity)
		//case RemoveInventoryItemFromOrder(_, inventoryItemId, quantity)	=> removeInventoryItemFromOrder(inventoryItemId)(quantity)
		//case AddShippingAddressToOrder(_, shippingAddress)				=> addShippingAddressToOrder(shippingAddress)
		//case PayForTheOrder(_) 											=> payTheBalance
		//case SubmitTheOrder(_)											=> submit
	//}

// 	private def eventEmissionForTransition[A : Aggregate](stateTransition: StateTransition[A]): List[Event] => List[Event] =
// 		evalTransition(stateTransition) compose AggregateRoot.rehydrated[A] _
// }

