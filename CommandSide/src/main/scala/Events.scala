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

// case class CreateOrder(id: UUID, customerId: UUID, customerName: UUID) extends Command
// case class AddInventoryItemToOrder(id: UUID, invetoryItemId: UUID, quantity: Int) extends Command
// case class RemoveInventoryItemFromOrder(id: UUID, invetoryItemId: UUID, quantity: Int) extends Command
// case class AddShippingAddressToOrder(id: UUID, invetoryItemId: UUID, quantity: Int) extends Command
// case class PayForTheOrder(id: UUID, invetoryItemId: UUID, quantity: Int) extends Command
// case class SubmitTheOrder(id: UUID) extends Command

//	Order
case class OrderCreated(id: UUID, name: String, sequence: Long) extends Event

//	Inventory Item
case class InventoryItemCreated(id: UUID, name: String, sequence: Long) extends Event
case class InventoryItemDeactivated(id: UUID, sequence: Long) extends Event
case class InventoryItemRenamed(id: UUID, newName: String, sequence: Long) extends Event
case class ItemsCheckedInToInventory(id: UUID, count: Int, sequence: Long) extends Event
case class ItemsRemovedFromInventory(id: UUID, count: Int, sequence: Long) extends Event

case class UnknownHappened(id: UUID, sequence: Long) extends Event