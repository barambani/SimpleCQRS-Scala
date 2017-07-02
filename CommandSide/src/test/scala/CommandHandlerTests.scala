package SimpleCqrsScala.CommandSide.Test

import java.util.UUID
import org.specs2.mutable._
import scala.collection.mutable._
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Application.Repository._
import SimpleCqrsScala.CommandSide.Application.CommandHandler._
import SimpleCqrsScala.CommandSide.Application.DomainCommandHandlers._
import SimpleCqrsScala.CommandSide.Printer._
import scalaz.ReaderT
import scalaz.concurrent.Task
import scalaz.{\/-, -\/}

sealed trait CommandHandlerStubs {

	lazy val id = UUID.randomUUID
	lazy val inventoryItemHistory = List(
		UnknownHappened(id, 6),
		ItemsRemovedFromInventory(id, 4, 5),
		ItemsRemovedFromInventory(id, 3, 4),
		InventoryItemRenamed(id, "Second Inventory Item Name", 3),
		ItemsCheckedInToInventory(id, 25, 2),
		InventoryItemCreated(id, "First Inventory Item Name", 1)
	)
	
	lazy val inventoryItemQuery: Query = 
		ReaderT { _ => Task.now(inventoryItemHistory) }

	lazy val orderQuery: Query = 
		ReaderT { 
			_ => Task.now { List(OrderCreated(id, "Test Order", 1)) }
		}

	def handleInTest[C](command: C, q: Query)(implicit CH: Handler[C]): Result = 
		CH.handle(command).run(q).unsafePerformSync
}

object CommandHandlerTests extends Specification with CommandHandlerStubs {
	
	"The Command Handler" should {

	  	"return an InventoryItemCreated event when receives the command CreateInventoryItem" in {

	  		val expectedName = "test-create-command"

  			lazy val evolution = handleInTest(
  				CreateInventoryItem(id, expectedName),
  				inventoryItemQuery
  			)

			evolution.toOption match {
				case Some(InventoryItemCreated(eid,name,sequence) :: xs) => {
					eid mustEqual id
					name mustEqual expectedName
					sequence mustEqual 1
				}
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an InventoryItemRenamed when receives the RenameInventoryItem command " in {

	  		val expectedName = "new item name"

	  		lazy val evolution = handleInTest(
	  			RenameInventoryItem(id, expectedName),
				inventoryItemQuery
	  		)
			
			evolution.toOption match {
				case Some(InventoryItemRenamed(eid,newName,sequence) :: xs) => {
					eid mustEqual id
					newName mustEqual expectedName
				}
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an ItemsCheckedInToInventory when receives the CheckInItemsToInventory command" in {

	  		val expectedCheckedInCount = 4

  			lazy val evolution = handleInTest(
  				CheckInItemsToInventory(id, expectedCheckedInCount),
  				inventoryItemQuery
  			)
  			
  			evolution.toOption match {
				case Some(ItemsCheckedInToInventory(eid,count,sequence) :: xs) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an RemoveItemsFromInventory when receives the RemoveItemsFromInventory command" in {

	  		val expectedCheckedInCount = 3

  			lazy val evolution = handleInTest(
  				RemoveItemsFromInventory(id, expectedCheckedInCount),
  				inventoryItemQuery
  			)
  			
  			evolution.toOption match {
				case Some(ItemsRemovedFromInventory(eid,count,sequence) :: xs) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an InventoryItemAddedToOrder when receives the AddInventoryItemToOrder command" in {

	  		val customerId = UUID.randomUUID
	  		val expectedItemsOfId = 12

  			lazy val evolution = handleInTest(
  				AddInventoryItemToOrder(id, customerId, expectedItemsOfId),
  				inventoryItemQuery
  			)
  			
  			evolution match {
				case \/-(InventoryItemAddedToOrder(id, inventoryItemId, quantity, sequence) :: xs) => quantity mustEqual expectedItemsOfId
				case -\/(es) => ko("The event saved into store is not correct: Errors -> $es")
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an error trying to add an empty shipping address to the order" in {

  			lazy val evolution = handleInTest(
  				AddShippingAddressToOrder(id, ""),
  				orderQuery
  			)

  			evolution.fold (
  				es 	=> show(es) mustEqual s"the shipping address '' proposed for the order 'Test Order' (id: ${id}) is not valid",
  				_	=> ko("The order shouldn't accept empty shipping address") 
  			)
	  	}
  	}
}
