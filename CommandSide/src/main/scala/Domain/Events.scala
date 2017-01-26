package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID

trait Identified {
	val id: UUID
}

sealed trait Sequenced {
	val sequence: Long
}

sealed trait Event extends Sequenced with Identified with Product with Serializable

object Event {
	
	lazy val hasACorrectId: Identified => Identity => Boolean = 
		event => aggregate => aggregate.id == new UUID(0, 0) || event.id == aggregate.id

	lazy val isInSequence: Sequenced => Versioned => Boolean = 
		event => aggregate => event.sequence == aggregate.expectedNextVersion
}

//	Order
final case class OrderCreated(id: UUID, description: String, sequence: Long) extends Event
final case class InventoryItemAddedToOrder(id: UUID, inventoryItemId: UUID, quantity: Int, sequence: Long) extends Event
final case class InventoryItemRemovedFromOrder(id: UUID, inventoryItemId: UUID, quantity: Int, sequence: Long) extends Event
final case class ShippingAddressAddedToOrder(id: UUID, shippingAddress: String, sequence: Long) extends Event
final case class OrderPayed(id: UUID, sequence: Long) extends Event
final case class OrderSubmitted(id: UUID, sequence: Long) extends Event
final case class OrderDispatched(id: UUID, sequence: Long) extends Event
final case class OrderVoided(id: UUID, reason: String, sequence: Long) extends Event

//	Inventory Item
final case class InventoryItemCreated(id: UUID, name: String, sequence: Long) extends Event
final case class InventoryItemDeactivated(id: UUID, sequence: Long) extends Event
final case class InventoryItemRenamed(id: UUID, newName: String, sequence: Long) extends Event
final case class ItemsCheckedInToInventory(id: UUID, count: Int, sequence: Long) extends Event
final case class ItemsRemovedFromInventory(id: UUID, count: Int, sequence: Long) extends Event

final case class UnknownHappened(id: UUID, sequence: Long) extends Event