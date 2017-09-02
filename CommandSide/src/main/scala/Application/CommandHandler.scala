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
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._

import scalaz.Reader
import scalaz.Kleisli
import scalaz.\/
import cats.effect._

trait Handler[C] {
  type A <: Identity
  def executionOf(c: C): EitherTransition[A]
}

object Handler {

  type AUX[C <: Command, OUT <: Identity] = Handler[C] { type A = OUT }

  def apply[C <: Command](implicit INST: Handler[C]): AUX[C, INST.A] = INST

  implicit class HandlerSyntax[C <: Command](c: C) {

    def handle[A <: Identity, CT <: CacheType, ST <: EventStoreType](cache: CT, store: ST)(
      implicit
        H:   Handler.AUX[C, A],
        AGG: Aggregate[A],
        CA:  CurrentAggregateState[CT, ST, A]): IO[Validated[(A, List[Event])]] =
      CA.fromCacheOrRehydrate.run(c.id) map { agg => H.executionOf(c) run agg } 
    }
}

object InventoryItemCommandHandlers extends InventoryItemService {

  import SimpleCqrsScala.CommandSide.Application.Handler._
  import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

  implicit lazy val createInventoryItemH: AUX[CreateInventoryItem, InventoryItem] =
    new Handler[CreateInventoryItem] {
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

object OrderCommandHandlers extends OrderService {

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
