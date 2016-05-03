package SimpleCqrsScala.CommandSide.Test.CommandHandlerTests

import java.util.UUID
import org.specs2.mutable._
import scala.collection.mutable._

import SimpleCqrsScala.CommandSide.Commands._
import SimpleCqrsScala.CommandSide.Events._
import SimpleCqrsScala.CommandSide.EventStore._
import SimpleCqrsScala.CommandSide.CommandHandler._

object CommandHandlerSpec extends Specification {

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
		lazy val initHandlerWithStore =  CommandHandler.apply _ compose TestEventStore.apply _

	  	"save an InventoryItemCreated event when receives the command CreateInventoryItem" in {

	  		val expectedName = "test-create-command"

	  		var store = MutableList[Event]()
	  		lazy val handler = initHandlerWithStore(store)

  			handler handle CreateInventoryItem(id, expectedName)

			store.head match {
				case InventoryItemCreated(eid,name,sequence) => {
					eid mustEqual id
					name mustEqual expectedName
					sequence mustEqual 1
				}
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"save an InventoryItemRenamed when receives the RenameInventoryItem command " in {

	  		val expectedName = "new item name"

	  		var store = MutableList[Event]() ++= history
	  		lazy val handler = initHandlerWithStore(store)

  			handler handle RenameInventoryItem(id, expectedName)
			
			store.head match {
				case InventoryItemRenamed(eid,newName,sequence) => newName mustEqual expectedName
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"save an ItemsCheckedInToInventory when receives the CheckInItemsToInventory command" in {

	  		val id = UUID.randomUUID
	  		val expectedCheckedInCount = 4

	  		var store = MutableList[Event]() ++= history
	  		lazy val handler = initHandlerWithStore(store)

  			handler handle CheckInItemsToInventory(id, expectedCheckedInCount)
  			
  			store.head match {
				case ItemsCheckedInToInventory(eid,count,sequence) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}

	  	"save an RemoveItemsFromInventory when receives the RemoveItemsFromInventory command" in {

	  		val id = UUID.randomUUID
	  		val expectedCheckedInCount = 3

			var store = MutableList[Event]() ++= history
	  		lazy val handler = initHandlerWithStore(store)

  			handler handle RemoveItemsFromInventory(id, expectedCheckedInCount)
  			
  			store.head match {
				case ItemsRemovedFromInventory(eid,count,sequence) => count mustEqual expectedCheckedInCount
				case _ => ko("The event saved into store is not correct")
			}
	  	}
  	}
}

private object TestEventStore {
	def apply(store: MutableList[Event]): TestEventStore = new TestEventStore(store)
}
private class TestEventStore(var store: MutableList[Event]) extends Repository {
	def Save(es: List[Event]): Unit = es.foreach(e => store.+=:(e))
	def GetHistoryById(id: UUID): List[Event] = (store filter (e => e.id == id)) toList
}
