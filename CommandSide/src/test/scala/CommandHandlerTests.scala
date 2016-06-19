package SimpleCqrsScala.CommandSide.Test

import java.util.UUID
import org.specs2.mutable._
import scala.collection.mutable._

import SimpleCqrsScala.CommandSide._

object CommandHandlerSpec extends Specification {

	import CommandHandler._

	"The Command Handler" should {

		val id = UUID.randomUUID
		val history = List(
			UnknownHappened(id, 6),
			ItemsRemovedFromInventory(id, 4, 5),
			ItemsRemovedFromInventory(id, 3, 4),
			InventoryItemRenamed(id, "Second Inventory Item Name", 3),
			ItemsCheckedInToInventory(id, 25, 2),
			InventoryItemCreated(id, "First Inventory Item Name", 1)
		)
		
		def eventStoreRetriever(idContainer: Identified): List[Event] = history
		def handleWithSideEffect = handle(eventStoreRetriever) _

	  	"return an InventoryItemCreated event when receives the command CreateInventoryItem" in {

	  		val expectedName = "test-create-command"

  			lazy val evolution = handleWithSideEffect(CreateInventoryItem(id, expectedName))

			evolution.head match {
				case InventoryItemCreated(eid,name,sequence) => {
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
			
			evolution.head match {
				case InventoryItemRenamed(eid,newName,sequence) => newName mustEqual expectedName
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an ItemsCheckedInToInventory when receives the CheckInItemsToInventory command" in {

	  		val id = UUID.randomUUID
	  		val expectedCheckedInCount = 4

  			lazy val evolution = handleWithSideEffect(CheckInItemsToInventory(id, expectedCheckedInCount))
  			
  			evolution.head match {
				case ItemsCheckedInToInventory(eid,count,sequence) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an RemoveItemsFromInventory when receives the RemoveItemsFromInventory command" in {

	  		val id = UUID.randomUUID
	  		val expectedCheckedInCount = 3

  			lazy val evolution = handleWithSideEffect(RemoveItemsFromInventory(id, expectedCheckedInCount))
  			
  			evolution.head match {
				case ItemsRemovedFromInventory(eid,count,sequence) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"return an InventoryItemAddedToOrder when receives the AddInventoryItemToOrder command" in {

	  		val id = UUID.randomUUID
	  		val customerId = UUID.randomUUID
	  		val expectedItemsOfId = 12

  			lazy val evolution = handleWithSideEffect(AddInventoryItemToOrder(id, customerId, expectedItemsOfId))
  			
  			evolution.head match {
				case InventoryItemAddedToOrder(id, inventoryItemId, quantity, sequence) => quantity mustEqual expectedItemsOfId
				case _ => ko("The event saved into store is not correct")
			}
	  	}
  	}
}
