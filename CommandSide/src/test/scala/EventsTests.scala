package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide._

import java.util.UUID

object EventSpec extends Specification {

  import Printer._

	val id = UUID.randomUUID

	"InventoryItemCreated" should {
  	"print the correct name, id and sequence number" in {
  		print(InventoryItemCreated(id, "test-name", 1)) mustEqual ("seq: 1 - Item test-name created (id:" + id.toString + ")")
  	}
	}

	"InventoryItemDeactivated" should {
  	"print the correct id and sequence number" in {
  		print(InventoryItemDeactivated(id, 12)) mustEqual ("seq: 12 - Item deactivated (id:" + id.toString + ")")
  	}
	}

	"InventoryItemRenamed" should {
  	"print the correct new name, id and sequence number" in {
  		print(InventoryItemRenamed(id, "new-test-name", 17)) mustEqual ("seq: 17 - Item renamed to new-test-name (id:" + id.toString + ")")
  	}
	}

	"ItemsCheckedInToInventory" should {
  	"print the correct amount, id and sequence number" in {
  		print(ItemsCheckedInToInventory(id, 3, 22)) mustEqual ("seq: 22 - Check-in of 3 item/s (id:" + id.toString + ")")
  	}
	}

	"ItemsRemovedFromInventory" should {
  	"print the correct amount, id and sequence number" in {
  		print(ItemsRemovedFromInventory(id, 2, 13)) mustEqual ("seq: 13 - Removed 2 item/s (id:" + id.toString + ")")
  	}
	}
}