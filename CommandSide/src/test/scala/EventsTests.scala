package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain._

import java.util.UUID

object EventSpec extends Specification {

  import Printer._

	val id = UUID.randomUUID

	"InventoryItemCreated" should {
  	"print the correct name, id and sequence number" in {
  		print(InventoryItemCreated(id, "test-name", 1)) mustEqual (s"seq: 1 - Item test-name created (id:$id)")
  	}
	}

	"InventoryItemDeactivated" should {
  	"print the correct id and sequence number" in {
  		print(InventoryItemDeactivated(id, 12)) mustEqual (s"seq: 12 - Item deactivated (id:$id)")
  	}
	}

	"InventoryItemRenamed" should {
  	"print the correct new name, id and sequence number" in {
  		print(InventoryItemRenamed(id, "new-test-name", 17)) mustEqual (s"seq: 17 - Item renamed to new-test-name (id:$id)")
  	}
	}

	"ItemsCheckedInToInventory" should {
  	"print the correct amount, id and sequence number" in {
  		print(ItemsCheckedInToInventory(id, 3, 22)) mustEqual (s"seq: 22 - Check-in of 3 item/s (id:$id)")
  	}
	}

	"ItemsRemovedFromInventory" should {
  	"print the correct amount, id and sequence number" in {
  		print(ItemsRemovedFromInventory(id, 2, 13)) mustEqual (s"seq: 13 - Removed 2 item/s (id:$id)")
  	}
	}
}