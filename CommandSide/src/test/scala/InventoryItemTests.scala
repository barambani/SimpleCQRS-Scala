package SimpleCqrsScala.CommandSide.Test.InventoryItemTests

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.InventoryItem._
import SimpleCqrsScala.CommandSide.Events._

import java.util.UUID

object InventoryItemSpec extends Specification {

	val id = UUID.randomUUID

	"InventoryItem" should {

	  	"have the correct state after the InventoryItemCreated event" in {

	  		val event = InventoryItemCreated(id, "Test Inventory Item", 1)

	  		val finalState = InventoryItem(event)

  			finalState.id mustEqual id
			finalState.name mustEqual "Test Inventory Item"
			finalState.isActivated mustEqual true
	  	}

	  	"change state correctly after receiving in sequence events" in {

	  		val history = List(
	  			InventoryItemDeactivated(id, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)

  			val finalState = InventoryItem(history)

  			finalState.version mustEqual 2
  			finalState.isActivated mustEqual false
	  	}

	  	"not change state after receiving out of sequence events" in {

	  		val history = List(
	  			InventoryItemDeactivated(id, 3),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)

  			val finalState = InventoryItem(history)

  			finalState.version mustEqual 1
  			finalState.isActivated mustEqual true
	  	}
	}
}