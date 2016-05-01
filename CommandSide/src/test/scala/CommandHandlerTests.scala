package SimpleCqrsScala.CommandSide.Test.CommandHandlerTests

import java.util.UUID
import org.specs2.mutable._

import SimpleCqrsScala.CommandSide.Commands._
import SimpleCqrsScala.CommandSide.Events._
import SimpleCqrsScala.CommandSide.EventStore._
import SimpleCqrsScala.CommandSide.CommandHandler._

object InventoryItemSpec extends Specification {

	"The Command Handler" should {

	  	"save an InventoryItemCreated event when receives the command CreateInventoryItem" in {

	  		val id = UUID.randomUUID

	  		def checkSavedEvent(e: Event): Unit = e match {
				case InventoryItemCreated(eid,name,sequence) => {
					eid mustEqual id
					name mustEqual "test-create-command"
					sequence mustEqual 1
				}
				case _ => ko("The saved Event is not correct")
			}

	  		lazy val handler = CommandHandler(TestEventStore(checkSavedEvent))

  			handler handle CreateInventoryItem(id, "test-create-command")
			ok
	  	}

	  	"save an Inventory Item Renamed " in {

	  		val id = UUID.randomUUID

	  		def checkSavedEvent(e: Event): Unit = e match {
				case InventoryItemCreated(eid,name,sequence) => {
					eid mustEqual id
					name mustEqual "test-create-command"
					sequence mustEqual 1
				}
				case _ => ko("The saved Event is not correct")
			}

	  		lazy val handler = CommandHandler(TestEventStore(checkSavedEvent))

  			handler handle CreateInventoryItem(id, "test-create-command")
			ok
	  	}
  	}
}

private object TestEventStore {
	def apply(saveChecks: Event => Unit): TestEventStore = new TestEventStore(saveChecks, _ => {}, List())
	def apply(saveChecks: Event => Unit, saveListChecks: List[Event] => Unit, history: List[Event]): TestEventStore = 
		new TestEventStore(saveChecks, saveListChecks, history)
}

private class TestEventStore(saveChecks: Event => Unit, saveListChecks: List[Event] => Unit, history: List[Event]) extends Repository {

	def Save(e: Event): Unit = saveChecks(e)
	def Save(es: List[Event]): Unit = saveListChecks(es)
	def GetHistoryById(id: UUID): List[Event] =
		List(
			ItemsRemovedFromInventory(id, 3, 4),
			InventoryItemRenamed(id, "Second Inventory Item Name", 3),
			ItemsCheckedInToInventory(id, 25, 2),
			InventoryItemCreated(id, "First Inventory Item Name", 1)
		)
}