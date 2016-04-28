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
	  		var savedEvent: Event = UnknownHappened(new UUID(0,0), 0)

	  		lazy val handler = CommandHandler(
	  			new TestEventStore(e => savedEvent = e, _ => {})
  			)

  			handler handle CreateInventoryItem(id, "test-create-command")

			savedEvent match {
				case InventoryItemCreated(eid,name,sequence) => {
					eid mustEqual id
					name mustEqual "test-create-command"
					sequence mustEqual 1
				}
				case _ => ko("The saved Event is not correct")
			}
	  	}
  	}
}

private class TestEventStore(saveSink: Event => Unit, saveListSink: List[Event] => Unit) extends Repository {
	def Save(e: Event): Unit = saveSink(e)
	def Save(es: List[Event]): Unit = saveListSink(es)
	def GetHistoryById(id: UUID): List[Event] = ???
}