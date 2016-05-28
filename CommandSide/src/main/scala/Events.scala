package SimpleCqrsScala.CommandSide

import java.util.UUID

sealed trait Identified {
	val id: UUID
}

sealed trait Sequenced {
	val sequence: Long
}

sealed trait Event extends Sequenced with Identified {
	def asHistory: List[Event] = List(this)
}

//	Order
case class OrderCreated(id: UUID, name: String, sequence: Long) extends Event
case class InventoryItemAddedToOrder(id: UUID, inventoryItemId: UUID, quantity: Int, sequence: Long) extends Event
case class InventoryItemRemovedFromOrder(id: UUID, inventoryItemId: UUID, quantity: Int, sequence: Long) extends Event
case class ShippingAddressAddedToOrder(id: UUID, shippingAddress: String, sequence: Long) extends Event
case class OrderPayed(id: UUID, sequence: Long) extends Event
case class OrderSubmitted(id: UUID, sequence: Long) extends Event

//	Inventory Item
case class InventoryItemCreated(id: UUID, name: String, sequence: Long) extends Event
case class InventoryItemDeactivated(id: UUID, sequence: Long) extends Event
case class InventoryItemRenamed(id: UUID, newName: String, sequence: Long) extends Event
case class ItemsCheckedInToInventory(id: UUID, count: Int, sequence: Long) extends Event
case class ItemsRemovedFromInventory(id: UUID, count: Int, sequence: Long) extends Event

case class UnknownHappened(id: UUID, sequence: Long) extends Event