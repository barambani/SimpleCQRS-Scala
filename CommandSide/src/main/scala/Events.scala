package SimpleCqrsScala.CommandSide

import java.util.UUID

object EventOps {
	import Domain._

	lazy val hasACorrectIdCheck: Identified => Identity => Boolean = 
		event => aggregate => aggregate.id == new UUID(0, 0) || event.id == aggregate.id

	lazy val isInSequenceCheck: Sequenced => Versioned => Boolean = 
		event => aggregate => event.sequence == nextStateVersion(aggregate)

	lazy val nextStateVersion: Versioned => Long = aggregate => aggregate.version + 1
}

trait Identified {
	val id: UUID
}

sealed trait Sequenced {
	val sequence: Long
}

sealed trait Event extends Sequenced with Identified with Product with Serializable

//	Order
final case class OrderCreated(id: UUID, description: String, sequence: Long) extends Event
final case class InventoryItemAddedToOrder(id: UUID, inventoryItemId: UUID, quantity: Int, sequence: Long) extends Event
final case class InventoryItemRemovedFromOrder(id: UUID, inventoryItemId: UUID, quantity: Int, sequence: Long) extends Event
final case class ShippingAddressAddedToOrder(id: UUID, shippingAddress: String, sequence: Long) extends Event
final case class OrderPayed(id: UUID, sequence: Long) extends Event
final case class OrderSubmitted(id: UUID, sequence: Long) extends Event

//	Inventory Item
final case class InventoryItemCreated(id: UUID, name: String, sequence: Long) extends Event
final case class InventoryItemDeactivated(id: UUID, sequence: Long) extends Event
final case class InventoryItemRenamed(id: UUID, newName: String, sequence: Long) extends Event
final case class ItemsCheckedInToInventory(id: UUID, count: Int, sequence: Long) extends Event
final case class ItemsRemovedFromInventory(id: UUID, count: Int, sequence: Long) extends Event

final case class UnknownHappened(id: UUID, sequence: Long) extends Event
