package EventsTests

import org.specs2.mutable._
import Events._

import java.util.UUID

object EventSpec extends Specification {

	val id = UUID.randomUUID

	"InventoryItemCreated" should {
  	"print the correct name, id and sequence number" in {
  		InventoryItemCreated(id, "test-name", 1).toString mustEqual ("seq: 1 - Item test-name created (id:" + id.toString + ")")
  	}
	}

	"InventoryItemDeactivated" should {
  	"print the correct id and sequence number" in {
  		InventoryItemDeactivated(id, 12).toString mustEqual ("seq: 12 - Item deactivated (id:" + id.toString + ")")
  	}
	}

	"InventoryItemRenamed" should {
  	"print the correct new name, id and sequence number" in {
  		InventoryItemRenamed(id, "new-test-name", 17).toString mustEqual ("seq: 17 - Item renamed to new-test-name (id:" + id.toString + ")")
  	}
	}

	"ItemsCheckedInToInventory" should {
  	"print the correct amount, id and sequence number" in {
  		ItemsCheckedInToInventory(id, 3, 22).toString mustEqual ("seq: 22 - Check-in of 3 item/s (id:" + id.toString + ")")
  	}
	}

	"ItemsRemovedFromInventory" should {
  	"print the correct amount, id and sequence number" in {
  		ItemsRemovedFromInventory(id, 2, 13).toString mustEqual ("seq: 13 - Removed 2 item/s (id:" + id.toString + ")")
  	}
	}
}