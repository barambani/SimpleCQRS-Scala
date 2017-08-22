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
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, InventoryItem, Identity}
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Services.OrderService
import SimpleCqrsScala.CommandSide.Services.InventoryItemService
import scalaz.Reader
import scalaz.Kleisli
import scalaz.\/
import cats.effect._

trait Handler[C] {

  type A <: Identity
  def executionOf(c: C): EitherTransition[A]

  def handle[CT <: CacheType, ST <: EventStoreType](c: C, id: UUID)(implicit CA: CurrentAggregateState[CT, ST, A]): IO[Validated[(A, List[Event])]] =
    CA.fromCacheOrRehydrate.run(id) map { agg => executionOf(c) run agg } 
}

object Handler {
  def apply[C](implicit instance: Handler[C]): Handler[C] = instance
}

object InventoryItemCommandHandlers extends InventoryItemService {

  import SimpleCqrsScala.CommandSide.Application.Handler._
  import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

  implicit object CreateInventoryItemH extends Handler[CreateInventoryItem] {
    type A = InventoryItem
    def executionOf(c: CreateInventoryItem): EitherTransition[InventoryItem] =
      createItemFor(c.id, c.name)
  }

  implicit object DeactivateInventoryItemH extends Handler[DeactivateInventoryItem] {
    type A = InventoryItem
    def executionOf(c: DeactivateInventoryItem): EitherTransition[InventoryItem] = deactivateInventoryItem
  }

  implicit object RenameInventoryItemH extends Handler[RenameInventoryItem] {
    type A = InventoryItem
    def executionOf(c: RenameInventoryItem): EitherTransition[InventoryItem] =
      renameInventoryItem(c.newName)
  }

  implicit object CheckInItemsToInventoryH extends Handler[CheckInItemsToInventory] {
    type A = InventoryItem
    def executionOf(c: CheckInItemsToInventory): EitherTransition[InventoryItem] =
      checkInItemsToInventory(c.count)
  }

  implicit object RemoveItemsFromInventoryH extends Handler[RemoveItemsFromInventory] {
    type A = InventoryItem
    def executionOf(c: RemoveItemsFromInventory): EitherTransition[InventoryItem] =
      removeItemsFromInventory(c.count)
  }
}

object OrderItemCommandHandlers extends OrderService {

  import SimpleCqrsScala.CommandSide.Application.Handler._
  import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

  implicit object CreateOrderH extends Handler[CreateOrder] {
    type A = Order
    def executionOf(c: CreateOrder): EitherTransition[Order] = 
      createOrderFor(c.id, c.customerId, c.customerName)
  }

  implicit object AddInventoryItemToOrderH extends Handler[AddInventoryItemToOrder] {
    type A = Order
    def executionOf(c: AddInventoryItemToOrder): EitherTransition[Order] = 
      addInventoryItemToOrder(c.inventoryItemId, c.quantity)
  }

  implicit object RemoveInventoryItemFromOrderH extends Handler[RemoveInventoryItemFromOrder] {
    type A = Order
    def executionOf(c: RemoveInventoryItemFromOrder): EitherTransition[Order] = 
      removeInventoryItemFromOrder(c.inventoryItemId, c.quantity)
  }

  implicit object AddShippingAddressToOrderH extends Handler[AddShippingAddressToOrder] {
    type A = Order
    def executionOf(c: AddShippingAddressToOrder): EitherTransition[Order] = 
      addShippingAddressToOrder(c.shippingAddress)
  }

  implicit object PayForTheOrderH extends Handler[PayForTheOrder] {
    type A = Order
    def executionOf(c: PayForTheOrder): EitherTransition[Order] = payTheBalance
  }

  implicit object SubmitTheOrderH extends Handler[SubmitTheOrder] {
    type A = Order
    def executionOf(c: SubmitTheOrder): EitherTransition[Order] = submit
  }
}
