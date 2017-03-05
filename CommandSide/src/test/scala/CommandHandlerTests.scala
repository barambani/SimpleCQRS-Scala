package SimpleCqrsScala.CommandSide.Test

import java.util.UUID
import org.specs2.mutable._
import scala.collection.mutable._
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Repository._
import SimpleCqrsScala.CommandSide.CommandHandler._
import SimpleCqrsScala.CommandSide.DomainCommandHandlers._
import scalaz.ReaderT
import scalaz.concurrent.Task

object CommandHandlerSpec extends Specification {

	val id = UUID.randomUUID
	val history = List(
		UnknownHappened(id, 6),
		ItemsRemovedFromInventory(id, 4, 5),
		ItemsRemovedFromInventory(id, 3, 4),
		InventoryItemRenamed(id, "Second Inventory Item Name", 3),
		ItemsCheckedInToInventory(id, 25, 2),
		InventoryItemCreated(id, "First Inventory Item Name", 1)
	)
	
	lazy val eventStoreQuery: Query = 
		ReaderT(_ => Task.now(history))
	
	def handleWithSideEffect[C](c: C)(implicit CH: Handler[C]): Result = 
		CH.handle(c).run(eventStoreQuery).unsafePerformSync

	"The Command Handler" should {

	  	"return an InventoryItemCreated event when receives the command CreateInventoryItem" in {

	  		val expectedName = "test-create-command"

  			lazy val evolution = handleWithSideEffect(CreateInventoryItem(id, expectedName))

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

	  		lazy val evolution = handleWithSideEffect(RenameInventoryItem(id, expectedName))
			
			evolution.toOption match {
				case Some(InventoryItemRenamed(eid,newName,sequence) :: xs) => newName mustEqual expectedName
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an ItemsCheckedInToInventory when receives the CheckInItemsToInventory command" in {

	  		val id = UUID.randomUUID
	  		val expectedCheckedInCount = 4

  			lazy val evolution = handleWithSideEffect(CheckInItemsToInventory(id, expectedCheckedInCount))
  			
  			evolution.toOption match {
				case Some(ItemsCheckedInToInventory(eid,count,sequence) :: xs) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an RemoveItemsFromInventory when receives the RemoveItemsFromInventory command" in {

	  		val id = UUID.randomUUID
	  		val expectedCheckedInCount = 3

  			lazy val evolution = handleWithSideEffect(RemoveItemsFromInventory(id, expectedCheckedInCount))
  			
  			evolution.toOption match {
				case Some(ItemsRemovedFromInventory(eid,count,sequence) :: xs) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an InventoryItemAddedToOrder when receives the AddInventoryItemToOrder command" in {

	  		val id = UUID.randomUUID
	  		val customerId = UUID.randomUUID
	  		val expectedItemsOfId = 12

  			lazy val evolution = handleWithSideEffect(AddInventoryItemToOrder(id, customerId, expectedItemsOfId))
  			
  			evolution.toOption match {
				case Some(InventoryItemAddedToOrder(id, inventoryItemId, quantity, sequence) :: xs) => quantity mustEqual expectedItemsOfId
				case _ => ko("The event saved into store is not correct")
			}
	  	}
  	}
}
