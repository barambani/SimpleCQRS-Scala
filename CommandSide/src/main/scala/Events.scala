package SimpleCqrsScala.CommandSide

import java.util.UUID

sealed trait Identity {
	val id: UUID
}

sealed trait Sequenced {
	val sequence: Long
}

sealed trait Event extends Sequenced with Identity {
	def asHistory: List[Event] = List(this)
}

//	Inventory Item
case class InventoryItemCreated(id: UUID, name: String, sequence: Long) extends Event
case class InventoryItemDeactivated(id: UUID, sequence: Long) extends Event
case class InventoryItemRenamed(id: UUID, newName: String, sequence: Long) extends Event
case class ItemsCheckedInToInventory(id: UUID, count: Int, sequence: Long) extends Event
case class ItemsRemovedFromInventory(id: UUID, count: Int, sequence: Long) extends Event

case class UnknownHappened(id: UUID, sequence: Long) extends Event
