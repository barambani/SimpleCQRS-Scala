package SimpleCqrsScala.CommandSide.Test

import java.util.UUID
import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.ErrorsPrint
import SimpleCqrsScala.CommandSide.Print.PrintSyntax
import SimpleCqrsScala.CommandSide.Application._
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, InventoryItem, Identity}
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Application.Handler._
import SimpleCqrsScala.CommandSide.Application.InventoryItemCommandHandlers._
import SimpleCqrsScala.CommandSide.Application.OrderCommandHandlers._
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._
import SimpleCqrsScala.CommandSide.Print.PrintSyntax
import SimpleCqrsScala.CommandSide.ErrorsPrint

import cats.effect.IO

import scalaz.Kleisli
import scalaz.ReaderT
import scalaz.{\/-, -\/}

sealed trait CommandHandlerStubs extends ErrorsPrint {

  lazy val itemId = UUID.randomUUID
  lazy val orderId = UUID.randomUUID

  lazy val testStore: Map[UUID, List[Event]] = Map(
    itemId -> List(
      UnknownHappened(itemId, 6),
      ItemsRemovedFromInventory(itemId, 4, 5),
      ItemsRemovedFromInventory(itemId, 3, 4),
      InventoryItemRenamed(itemId, "Second Inventory Item Name", 3),
      ItemsCheckedInToInventory(itemId, 25, 2),
      InventoryItemCreated(itemId, "First Inventory Item Name", 1)
    ),
    orderId -> List(OrderCreated(orderId, "Test Order", 1))
  )

  val orderTestCache = new Cache[LocalActor, Order] {

    def read: CacheGet[Order] = 
      Kleisli { id => IO { None } }

    def write: CachePut[Order] =
      Kleisli { a => IO { () } }
  }

  val inventoryItemTestCache = new Cache[LocalActor, InventoryItem] {

    def read: CacheGet[InventoryItem] = 
      Kleisli { id => IO { None } }

    def write: CachePut[InventoryItem] =
      Kleisli { a => IO { () } }
  }

  val testEventStore = new EventStore[Cassandra] {
    def read: StoreRetrieve =
      Kleisli { id => IO { testStore.getOrElse(id, Nil) } }

    def write: StoreInsert =
      Kleisli { events => IO { () } }
  }

  implicit val currentTestOrderState: CurrentAggregateState[LocalActor, Cassandra, Order] = 
    new CurrentAggregateState[LocalActor, Cassandra, Order] {
      val CA = orderTestCache
      val ES = testEventStore
      val AGG = Aggregate[Order]
    }

  implicit val currentTestInventoryItemState: CurrentAggregateState[LocalActor, Cassandra, InventoryItem] = 
    new CurrentAggregateState[LocalActor, Cassandra, InventoryItem] {
      val CA = inventoryItemTestCache
      val ES = testEventStore
      val AGG = Aggregate[InventoryItem]
    }
  
  def handleInTest[C <: Command, A <: Identity](command: C)(
    implicit
      H: Handler.AUX[C, A],
      AGG: Aggregate[A],
      CA: CurrentAggregateState[LocalActor, Cassandra, A]): Validated[(A, List[Event])] =
    command.handle(LocalActor, Cassandra).unsafeRunSync
}

object CommandHandlerTests extends Specification with CommandHandlerStubs with ErrorsPrint {

  "The Command Handler" should {

    "return an InventoryItemCreated event when receives the command CreateInventoryItem" in {

      val otherId = UUID.randomUUID
      val expectedName = "test-create-command"

      lazy val evolution = handleInTest(CreateInventoryItem(otherId, expectedName))

      evolution.toOption match {
        case Some((item,  InventoryItemCreated(eid, name, sequence) :: xs)) =>
          eid mustEqual otherId
          name mustEqual expectedName
          sequence mustEqual 1
          item.id mustEqual otherId
          item.name mustEqual expectedName
          item.itemsCount mustEqual 0
          item.version mustEqual 1
        case None => ko(s"The command application failed. Error: ${ evolution leftMap { _.print } }")
        case _  => ko("The event saved into store is not correct")
      }
    }

    "return an InventoryItemRenamed when receives the RenameInventoryItem command " in {

      val expectedName = "new item name"

      lazy val evolution = handleInTest(RenameInventoryItem(itemId, expectedName))

      evolution.toOption match {
        case Some((item, InventoryItemRenamed(eid,newName,sequence) :: xs)) => {
          eid mustEqual itemId
          newName mustEqual expectedName
        }
        case _ => ko("The event saved into store is not correct")
      }
    }

    "return an ItemsCheckedInToInventory when receives the CheckInItemsToInventory command" in {

      val expectedCheckedInCount = 4

      lazy val evolution = handleInTest(CheckInItemsToInventory(itemId, expectedCheckedInCount))

      evolution.toOption match {
        case Some((item, ItemsCheckedInToInventory(eid,count,sequence) :: xs)) => count mustEqual expectedCheckedInCount
        case _ => ko("The event saved into store is not correct")
      }
    }

    "return an RemoveItemsFromInventory when receives the RemoveItemsFromInventory command" in {

      val expectedCheckedInCount = 3

      lazy val evolution = handleInTest(RemoveItemsFromInventory(itemId, expectedCheckedInCount))

      evolution.toOption match {
        case Some((item, ItemsRemovedFromInventory(eid,count,sequence) :: xs)) => count mustEqual expectedCheckedInCount
        case _ => ko("The event saved into store is not correct")
      }
    }

    "return an InventoryItemAddedToOrder when receives the AddInventoryItemToOrder command" in {

      val customerId = UUID.randomUUID
      val expectedItemsOfId = 12

      lazy val evolution = handleInTest(AddInventoryItemToOrder(orderId, customerId, expectedItemsOfId))

      evolution match {
        case \/-((order, InventoryItemAddedToOrder(id, inventoryItemId, quantity, sequence) :: xs)) => quantity mustEqual expectedItemsOfId
        case -\/(es) => ko("The event saved into store is not correct: Errors -> $es")
        case _ => ko("The event saved into store is not correct")
      }
    }

    "return an error trying to add an empty shipping address to the order" in {

      lazy val evolution = handleInTest(AddShippingAddressToOrder(orderId, ""))

      evolution.fold (
        es  => es.print mustEqual s"the shipping address '' proposed for the order 'Test Order' (id: ${orderId}) is not valid",
        _   => ko("The order shouldn't accept empty shipping address") 
      )
    }
  }
}
