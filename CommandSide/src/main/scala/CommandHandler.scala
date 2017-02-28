package SimpleCqrsScala.CommandSide

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain._
import scalaz.Reader
import scalaz.\/
import Repository._
import DomainState._
import EitherTransition._
import scalaz.concurrent.Task
import SimpleCqrsScala.CommandSide.Domain.InventoryItem._
import SimpleCqrsScala.CommandSide.Domain.Order._

object CommandHandler {

	type Result 		= \/[ErrorMessage, List[Event]]
	type CommandEffect	= Reader[Query, Task[Result]]

	trait Handler[C] {
		def handle(c: C): CommandEffect
	}

	object Handler {

		def apply[C](implicit instance: Handler[C]): Handler[C] = instance

		def transitionAfterState[S: Aggregate](t: S => EitherTransition[S])(s: S): Task[Result] = 
			Task.now(evalTransition(t(s))(s))

		def transitionAfterHistory[S: Aggregate](t: S => EitherTransition[S])(h: List[Event]): Task[Result] = 
			transitionAfterState(t)(Aggregate[S].rehydrate(h))

		def initialEffectOf[S: Aggregate](t: EitherTransition[S]): CommandEffect = 
			Reader{ _ => transitionAfterHistory[S](_ => t)(Nil) }

		def effectForS[S: Aggregate](t: S => EitherTransition[S])(id: UUID): CommandEffect = 
			Reader{ q => (q andThenK transitionAfterHistory(t)).run(id) }

		def effectFor[S: Aggregate](t: EitherTransition[S])(id: UUID): CommandEffect = 
			effectForS[S](_ => t)(id)
	}
}

object DomainCommandHandlers {

	import CommandHandler._
	import Handler._
	import DomainAggregates._

	implicit object CreateInventoryItemH extends Handler[CreateInventoryItem] {
		def handle(c: CreateInventoryItem): CommandEffect = 
			initialEffectOf(InventoryItem.createFor(c.id)(c.name))
	}
	
	implicit object DeactivateInventoryItemH extends Handler[DeactivateInventoryItem] {
		def handle(c: DeactivateInventoryItem): CommandEffect = 
			effectFor(deactivateInventoryItem)(c.id)
	}

	implicit object RenameInventoryItemH extends Handler[RenameInventoryItem] {
		def handle(c: RenameInventoryItem): CommandEffect = 
			effectForS(renameInventoryItem(c.newName))(c.id)
	}

	implicit object CheckInItemsToInventoryH extends Handler[CheckInItemsToInventory] {
		def handle(c: CheckInItemsToInventory): CommandEffect = 
			effectFor(checkInItemsToInventory(c.count))(c.id)
	}

	implicit object RemoveItemsFromInventoryH extends Handler[RemoveItemsFromInventory] {
		def handle(c: RemoveItemsFromInventory): CommandEffect = 
			effectForS(removeItemsFromInventory(c.count))(c.id)
	}



	implicit object CreateOrderH extends Handler[CreateOrder] {
		def handle(c: CreateOrder): CommandEffect = 
			initialEffectOf(Order.createFor(c.id)(c.customerId)(c.customerName))
	}

	implicit object AddInventoryItemToOrderH extends Handler[AddInventoryItemToOrder] {
		def handle(c: AddInventoryItemToOrder): CommandEffect = 
			effectForS(addInventoryItemToOrder(c.inventoryItemId)(c.quantity))(c.id)
	}

	implicit object RemoveInventoryItemFromOrderH extends Handler[RemoveInventoryItemFromOrder] {
		def handle(c: RemoveInventoryItemFromOrder): CommandEffect = 
			effectForS(removeInventoryItemFromOrder(c.inventoryItemId)(c.quantity))(c.id)
	}

	implicit object AddShippingAddressToOrderH extends Handler[AddShippingAddressToOrder] {
		def handle(c: AddShippingAddressToOrder): CommandEffect = 
			effectForS(addShippingAddressToOrder(c.shippingAddress))(c.id)
	}

	implicit object PayForTheOrderH extends Handler[PayForTheOrder] {
		def handle(c: PayForTheOrder): CommandEffect = 
			effectForS(payTheBalance)(c.id)
	}

	implicit object SubmitTheOrderH extends Handler[SubmitTheOrder] {
		def handle(c: SubmitTheOrder): CommandEffect = 
			effectForS(submit)(c.id)
	}
}