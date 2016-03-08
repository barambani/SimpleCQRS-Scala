package InventoryItemTests

import org.specs2.mutable._
import InventoryItem._

import java.util.UUID

object InventoryItemSpec extends Specification {

	val id = UUID.randomUUID

	"InventoryItem" should {
	  	"contain only the InventoryItemCreated event when created" in {

	  		val inventoryItem = InventoryItem(id, "Test Inventory Item")

			inventoryItem.getUncommittedChanges.size mustEqual 1	  		
	  	}
	}
}