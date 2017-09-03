package SimpleCqrsScala.CommandSide.Application

import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, InventoryItem, Identity}
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Services.OrderService
import SimpleCqrsScala.CommandSide.Services.InventoryItemService
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._

import scala.language.higherKinds
import scalaz.Monad
import scalaz.syntax.monad._

trait Handler[C] {
  type A <: Identity
  def executionOf(c: C): EitherTransition[A]
}

object Handler {

  type AUX[C <: Command, OUT <: Identity] = Handler[C] { type A = OUT }

  def apply[C <: Command](implicit INST: Handler[C]): AUX[C, INST.A] = INST

  implicit class HandlerSyntax[C <: Command, A <: Identity](c: C)(
    implicit
      H:   Handler.AUX[C, A],
      AGG: Aggregate[A]) {

    def handle[CT <: CacheType, ST <: EventStoreType, F[_]](
      implicit
        CA:  CurrentAggregateState[CT, ST, A],
        MO:  Monad[F]): F[Validated[(A, List[Event])]] =
      CA.fromCacheOrRehydrate.run(c.id) map { agg => H.executionOf(c) run agg } 
  }
}

object InventoryItemCommandHandlers extends InventoryItemService {

  import SimpleCqrsScala.CommandSide.Application.Handler._

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
