package SimpleCqrsScala.CommandSide.Application

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.InventoryItem._
import SimpleCqrsScala.CommandSide.Domain.Order._
import SimpleCqrsScala.CommandSide.Domain.Errors._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.Events.Event._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import SimpleCqrsScala.CommandSide.Domain.DomainState.EitherTransition._
import SimpleCqrsScala.CommandSide.Application.Repository._
import SimpleCqrsScala.CommandSide.Domain.Aggregate
import SimpleCqrsScala.CommandSide.Services.OrderService
import SimpleCqrsScala.CommandSide.Services.InventoryItemService
import scalaz.Reader
import scalaz.\/
import scalaz.NonEmptyList
import scalaz.concurrent.Task

object CommandHandler {

	type Result 		= \/[NonEmptyList[ErrorMessage], List[Event]]
	type CommandEffect	= Reader[Query, Task[Result]]

	trait Handler[C] {
		def handle(c: C): CommandEffect
	}

	object Handler {

		def apply[C](implicit instance: Handler[C]): Handler[C] = instance

		def transitionAfterState[S: Aggregate](t: EitherTransition[S])(s: S): Task[Result] = 
			Task.now(evalTransition(t)(s))

		def transitionAfterHistory[S: Aggregate](t: EitherTransition[S])(h: List[Event]): Task[Result] = 
			transitionAfterState(t)(Aggregate[S].rehydrate(h))

		def initialEffectOf[S: Aggregate](t: EitherTransition[S]): CommandEffect = 
			Reader { _ => transitionAfterHistory[S](t)(Nil) }

		def effectFor[S: Aggregate](t: EitherTransition[S])(id: UUID): CommandEffect = 
			Reader { q => (q andThenK transitionAfterHistory(t)).run(id) }
	}
}

object DomainCommandHandlers extends OrderService with InventoryItemService {

	import SimpleCqrsScala.CommandSide.Application.CommandHandler._
	import SimpleCqrsScala.CommandSide.Application.CommandHandler.Handler._
	import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

	implicit object CreateInventoryItemH extends Handler[CreateInventoryItem] {
		def handle(c: CreateInventoryItem): CommandEffect = 
			initialEffectOf(createItemFor(c.id, c.name))
	}
	
	implicit object DeactivateInventoryItemH extends Handler[DeactivateInventoryItem] {
		def handle(c: DeactivateInventoryItem): CommandEffect = 
			effectFor(deactivateInventoryItem)(c.id)
	}

	implicit object RenameInventoryItemH extends Handler[RenameInventoryItem] {
		def handle(c: RenameInventoryItem): CommandEffect = 
			effectFor(renameInventoryItem(c.newName))(c.id)
	}

	implicit object CheckInItemsToInventoryH extends Handler[CheckInItemsToInventory] {
		def handle(c: CheckInItemsToInventory): CommandEffect = 
			effectFor(checkInItemsToInventory(c.count))(c.id)
	}

	implicit object RemoveItemsFromInventoryH extends Handler[RemoveItemsFromInventory] {
		def handle(c: RemoveItemsFromInventory): CommandEffect = 
			effectFor(removeItemsFromInventory(c.count))(c.id)
	}



	implicit object CreateOrderH extends Handler[CreateOrder] {
		def handle(c: CreateOrder): CommandEffect = 
			initialEffectOf(createOrderFor(c.id, c.customerId, c.customerName))
	}

	implicit object AddInventoryItemToOrderH extends Handler[AddInventoryItemToOrder] {
		def handle(c: AddInventoryItemToOrder): CommandEffect = 
			effectFor(addInventoryItemToOrder(c.inventoryItemId, c.quantity))(c.id)
	}

	implicit object RemoveInventoryItemFromOrderH extends Handler[RemoveInventoryItemFromOrder] {
		def handle(c: RemoveInventoryItemFromOrder): CommandEffect = 
			effectFor(removeInventoryItemFromOrder(c.inventoryItemId, c.quantity))(c.id)
	}

	implicit object AddShippingAddressToOrderH extends Handler[AddShippingAddressToOrder] {
		def handle(c: AddShippingAddressToOrder): CommandEffect = 
			effectFor(addShippingAddressToOrder(c.shippingAddress))(c.id)
	}

	implicit object PayForTheOrderH extends Handler[PayForTheOrder] {
		def handle(c: PayForTheOrder): CommandEffect = 
			effectFor(payTheBalance)(c.id)
	}

	implicit object SubmitTheOrderH extends Handler[SubmitTheOrder] {
		def handle(c: SubmitTheOrder): CommandEffect = 
			effectFor(submit)(c.id)
	}
}