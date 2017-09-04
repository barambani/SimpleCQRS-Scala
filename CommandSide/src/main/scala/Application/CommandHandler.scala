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

  def handle[C <: Command, A <: Identity, CT <: CacheType, ST <: EventStoreType, F[_]](c: C)(
    implicit
      H  : Handler.AUX[C, A],
      CA : Cache[CT, A],
      ES : EventStore[ST],
      AGG: Aggregate[A],
      MO : Monad[F]): F[Validated[(A, List[Event])]] =
    CurrentAggregateState[A].fromCacheOrRehydrate.run(c.id) map { agg => H.executionOf(c) run agg }

  implicit class HandlerSyntax[C <: Command, A <: Identity](c: C) {
    def handle[CT <: CacheType, ST <: EventStoreType, F[_]](
      implicit
        H  : Handler.AUX[C, A],
        CA : Cache[CT, A],
        ES : EventStore[ST],
        AGG: Aggregate[A],
        MO : Monad[F]): F[Validated[(A, List[Event])]] =
      Handler.handle(c)
  }
}

object BuiltInCommandHandlers extends CassandraStore with RedisEventStore {

  import SimpleCqrsScala.CommandSide.Application.Handler.HandlerSyntax

  implicit class BuiltInCommandHandlersSyntax[C <: Command, A <: Identity](c: C) {
    
    def handleCassandra[F[_]](
      implicit 
        H  : Handler.AUX[C, A],
        CA : Cache[LocalActor, A],
        AGG: Aggregate[A],
        MO : Monad[F]): F[Validated[(A, List[Event])]] =
      c.handle[LocalActor, Cassandra, F]

    def handleCassandraDist[F[_]](
      implicit 
        H  : Handler.AUX[C, A],
        CA : Cache[Memcached, A],
        AGG: Aggregate[A],
        MO : Monad[F]): F[Validated[(A, List[Event])]] =
      c.handle[Memcached, Cassandra, F]

    def handleRedis[F[_]](
      implicit 
        H  : Handler.AUX[C, A],
        CA : Cache[LocalActor, A],
        AGG: Aggregate[A],
        MO : Monad[F]): F[Validated[(A, List[Event])]] =
      c.handle[LocalActor, Redis, F]

    def handleRediasDist[F[_]](
      implicit 
        H  : Handler.AUX[C, A],
        CA : Cache[Memcached, A],
        AGG: Aggregate[A],
        MO : Monad[F]): F[Validated[(A, List[Event])]] =
      c.handle[Memcached, Redis, F]
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
