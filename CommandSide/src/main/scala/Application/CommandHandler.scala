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
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, Identity}
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Services.OrderService
import SimpleCqrsScala.CommandSide.Services.InventoryItemService
import scalaz.Reader
import scalaz.Kleisli
import scalaz.\/
import cats.effect._

object CommandHandler {

	trait Handler[Comm] {

		type A <: Identity

		def executionOf(c: Comm): EitherTransition[A]

		def handle[C <: CacheType, S <: EventStoreType](c: Comm, id: UUID)(implicit CA: CurrentAggregate[C, S, A]): IO[Validated[(A, List[Event])]] =
			CA.fromCacheOrRehydrate.run(id) map { agg => executionOf(c) run agg } 
	}

	object Handler {
		def apply[Comm](implicit instance: Handler[Comm]): Handler[Comm] = instance
	}
}

object DomainCommandHandlers extends OrderService with InventoryItemService {

	import SimpleCqrsScala.CommandSide.Application.CommandHandler._
	import SimpleCqrsScala.CommandSide.Application.CommandHandler.Handler._
	import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

	// implicit object CreateInventoryItemH extends Handler[CreateInventoryItem] {
	// 	def handle(c: CreateInventoryItem): CommandEffect = 
	// 		initialEffectOf(createItemFor(c.id, c.name))
	// }
	
	// implicit object DeactivateInventoryItemH extends Handler[DeactivateInventoryItem] {
	// 	def handle(c: DeactivateInventoryItem): CommandEffect = 
	// 		effectFor(deactivateInventoryItem)(c.id)
	// }

	// implicit object RenameInventoryItemH extends Handler[RenameInventoryItem] {
	// 	def handle(c: RenameInventoryItem): CommandEffect = 
	// 		effectFor(renameInventoryItem(c.newName))(c.id)
	// }

	// implicit object CheckInItemsToInventoryH extends Handler[CheckInItemsToInventory] {
	// 	def handle(c: CheckInItemsToInventory): CommandEffect = 
	// 		effectFor(checkInItemsToInventory(c.count))(c.id)
	// }

	// implicit object RemoveItemsFromInventoryH extends Handler[RemoveItemsFromInventory] {
	// 	def handle(c: RemoveItemsFromInventory): CommandEffect = 
	// 		effectFor(removeItemsFromInventory(c.count))(c.id)
	// }



	// implicit object CreateOrderH extends Handler[CreateOrder] {
	// 	def handle(c: CreateOrder): CommandEffect = 
	// 		initialEffectOf(createOrderFor(c.id, c.customerId, c.customerName))
	// }

	implicit object AddInventoryItemToOrderH extends Handler[AddInventoryItemToOrder] {
		type A = Order
		def executionOf(c: AddInventoryItemToOrder): EitherTransition[Order] = 
			addInventoryItemToOrder(c.inventoryItemId, c.quantity)
	}

	// implicit object RemoveInventoryItemFromOrderH extends Handler[RemoveInventoryItemFromOrder] {
	// 	def handle(c: RemoveInventoryItemFromOrder): CommandEffect = 
	// 		effectFor(removeInventoryItemFromOrder(c.inventoryItemId, c.quantity))(c.id)
	// }

	// implicit object AddShippingAddressToOrderH extends Handler[AddShippingAddressToOrder] {
	// 	def handle(c: AddShippingAddressToOrder): CommandEffect = 
	// 		effectFor(addShippingAddressToOrder(c.shippingAddress))(c.id)
	// }

	// implicit object PayForTheOrderH extends Handler[PayForTheOrder] {
	// 	def handle(c: PayForTheOrder): CommandEffect = 
	// 		effectFor(payTheBalance)(c.id)
	// }

	// implicit object SubmitTheOrderH extends Handler[SubmitTheOrder] {
	// 	def handle(c: SubmitTheOrder): CommandEffect = 
	// 		effectFor(submit)(c.id)
	// }
}