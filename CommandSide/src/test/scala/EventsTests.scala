package EventsTests

import org.specs2.mutable._
import Events._

import java.util.UUID

object EventSpec extends Specification {

	val id = UUID.randomUUID

	"InventoryItemCreated" should {
  	"print the correct name and id" in {
  		InventoryItemCreated(id, "test-name").toString mustEqual ("Item test-name created (id:" + id.toString + ")")
  	}
	}

	"InventoryItemDeactivated" should {
  	"print the correct id" in {
  		InventoryItemDeactivated(id).toString mustEqual ("Item deactivated (id:" + id.toString + ")")
  	}
	}

	"InventoryItemRenamed" should {
  	"print the correct new name and id" in {
  		InventoryItemRenamed(id, "new-test-name").toString mustEqual ("Item renamed to new-test-name (id:" + id.toString + ")")
  	}
	}

	"ItemsCheckedInToInventory" should {
  	"print the correct amount and id" in {
  		ItemsCheckedInToInventory(id, 3).toString mustEqual ("Check-in of 3 item/s (id:" + id.toString + ")")
  	}
	}

	"ItemsRemovedFromInventory" should {
  	"print the correct amount and id" in {
  		ItemsRemovedFromInventory(id, 2).toString mustEqual ("Removed 2 item/s (id:" + id.toString + ")")
  	}
	}
}