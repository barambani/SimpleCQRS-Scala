package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.{OrderEventsPrint, InventoryItemEventsPrint, ErrorsPrint}
import SimpleCqrsScala.CommandSide.Print.PrintSyntax
import java.util.UUID

object PrintTests extends Specification with ErrorsPrint with OrderEventsPrint with InventoryItemEventsPrint {

  val id = UUID.randomUUID
  val id2 = UUID.randomUUID

  "OrderCreated" should {
    "print the correct properties" in {
      OrderCreated(id, "test-description", 1).print mustEqual (s"seq: 1 - Order test-description created (id: $id)")
    }
  }

  "InventoryItemAddedToOrder" should {
    "print the correct properties" in {
      InventoryItemAddedToOrder(id, id2, 2, 1).print mustEqual (s"seq: 1 - 2 of inventory item $id2 added to order (id: $id)")
    }
  }

  "InventoryItemRemovedFromOrder" should {
    "print the correct properties" in {
      InventoryItemRemovedFromOrder(id, id2, 3, 1).print mustEqual (s"seq: 1 - 3 of inventory item $id2 removed from order (id: $id)")
    }
  }

  "ShippingAddressAddedToOrder" should {
    "print the correct properties" in {
      ShippingAddressAddedToOrder(id, "test address", 1).print mustEqual (s"seq: 1 - address: 'test address' added order (id: $id)")
    }
  }

  "OrderPayed" should {
    "print the correct properties" in {
      OrderPayed(id, 1).print mustEqual (s"seq: 1 - order payed (id: $id)")
    }
  }

  "OrderSubmitted" should {
    "print the correct properties" in {
      OrderSubmitted(id, 1).print mustEqual (s"seq: 1 - order submitted (id: $id)")
    }
  }


  "InventoryItemCreated" should {
    "print the correct name, id and sequence number" in {
      InventoryItemCreated(id, "test-name", 1).print mustEqual (s"seq: 1 - Item test-name created (id: $id)")
    }
  }

  "InventoryItemDeactivated" should {
    "print the correct id and sequence number" in {
      InventoryItemDeactivated(id, 12).print mustEqual (s"seq: 12 - Item deactivated (id: $id)")
    }
  }

  "InventoryItemRenamed" should {
    "print the correct new name, id and sequence number" in {
      InventoryItemRenamed(id, "new-test-name", 17).print mustEqual (s"seq: 17 - Item renamed to new-test-name (id: $id)")
    }
  }

  "ItemsCheckedInToInventory" should {
    "print the correct amount, id and sequence number" in {
      ItemsCheckedInToInventory(id, 3, 22).print mustEqual (s"seq: 22 - Check-in of 3 item/s (id: $id)")
    }
  }

  "ItemsRemovedFromInventory" should {
    "print the correct amount, id and sequence number" in {
      ItemsRemovedFromInventory(id, 2, 13).print mustEqual (s"seq: 13 - Removed 2 item/s (id: $id)")
    }
  }
}
