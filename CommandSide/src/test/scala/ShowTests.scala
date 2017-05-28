package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Printer._
import java.util.UUID

object ShowTests extends Specification {

	val id = UUID.randomUUID
  val id2 = UUID.randomUUID

  "OrderCreated" should {
    "show the correct properties" in {
      show(OrderCreated(id, "test-description", 1)) mustEqual (s"seq: 1 - Order test-description created (id: $id)")
    }
  }

  "InventoryItemAddedToOrder" should {
    "show the correct properties" in {
      show(InventoryItemAddedToOrder(id, id2, 2, 1)) mustEqual (s"seq: 1 - 2 of inventory item $id2 added to order (id: $id)")
    }
  }

  "InventoryItemRemovedFromOrder" should {
    "show the correct properties" in {
      show(InventoryItemRemovedFromOrder(id, id2, 3, 1)) mustEqual (s"seq: 1 - 3 of inventory item $id2 removed from order (id: $id)")
    }
  }

  "ShippingAddressAddedToOrder" should {
    "show the correct properties" in {
      show(ShippingAddressAddedToOrder(id, "test address", 1)) mustEqual (s"seq: 1 - address: 'test address' added order (id: $id)")
    }
  }

  "OrderPayed" should {
    "show the correct properties" in {
      show(OrderPayed(id, 1)) mustEqual (s"seq: 1 - order payed (id: $id)")
    }
  }

  "OrderSubmitted" should {
    "show the correct properties" in {
      show(OrderSubmitted(id, 1)) mustEqual (s"seq: 1 - order submitted (id: $id)")
    }
  }


	"InventoryItemCreated" should {
    "show the correct name, id and sequence number" in {
  		show(InventoryItemCreated(id, "test-name", 1)) mustEqual (s"seq: 1 - Item test-name created (id: $id)")
  	}
	}

	"InventoryItemDeactivated" should {
  	"show the correct id and sequence number" in {
  		show(InventoryItemDeactivated(id, 12)) mustEqual (s"seq: 12 - Item deactivated (id: $id)")
  	}
	}

	"InventoryItemRenamed" should {
  	"show the correct new name, id and sequence number" in {
  		show(InventoryItemRenamed(id, "new-test-name", 17)) mustEqual (s"seq: 17 - Item renamed to new-test-name (id: $id)")
  	}
	}

	"ItemsCheckedInToInventory" should {
  	"show the correct amount, id and sequence number" in {
  		show(ItemsCheckedInToInventory(id, 3, 22)) mustEqual (s"seq: 22 - Check-in of 3 item/s (id: $id)")
  	}
	}

	"ItemsRemovedFromInventory" should {
  	"show the correct amount, id and sequence number" in {
  		show(ItemsRemovedFromInventory(id, 2, 13)) mustEqual (s"seq: 13 - Removed 2 item/s (id: $id)")
  	}
	}
}