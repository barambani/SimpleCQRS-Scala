package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide._

import java.util.UUID

object InventoryItemSpec extends Specification {

	val id = UUID.randomUUID

	"InventoryItem" should {

	  	"have the correct state after the InventoryItemCreated event" in {

	  		val event = InventoryItemCreated(id, "Test Inventory Item", 1)

	  		val finalState = InventoryItem(event.asHistory)

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

	  	"have the correct name after the InventoryItemRenamed event" in {

  			val history = List(
	  			InventoryItemRenamed(id, "New Iten Name", 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		val finalState = InventoryItem(history)

			finalState.name mustEqual "New Iten Name"
	  	}

	  	"have the correct item count after the check in and removal of items" in {

  			val history = List(
  				ItemsRemovedFromInventory(id, 3, 5),
  				ItemsCheckedInToInventory(id, 2, 4),
  				ItemsRemovedFromInventory(id, 3, 3),
  				ItemsCheckedInToInventory(id, 10, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		val finalState = InventoryItem(history)

			finalState.itemsCount mustEqual 6
	  	}
	}
}