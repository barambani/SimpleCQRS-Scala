package SimpleCqrsScala.CommandSide.Events

import java.util.UUID

sealed trait Sequenced {
	val sequence: Long
}

sealed trait Event extends Sequenced {
	override def toString: String = this match {
		case InventoryItemCreated(id, name, sequence) => "seq: " + sequence + " - Item " + name + " created (id:" + id.toString() + ")"
		case InventoryItemDeactivated(id, sequence) => "seq: " + sequence + " - Item deactivated (id:" + id.toString() + ")"
		case InventoryItemRenamed(id, newName, sequence) => "seq: " + sequence + " - Item renamed to " + newName + " (id:" + id.toString() + ")"
		case ItemsCheckedInToInventory(id, count, sequence) => "seq: " + sequence + " - Check-in of " + count.toString() + " item/s (id:" + id.toString() + ")"
		case ItemsRemovedFromInventory(id, count, sequence) => "seq: " + sequence + " - Removed " + count.toString() + " item/s (id:" + id.toString() + ")"
		case _ => ""
	} 
}

case class InventoryItemCreated(id: UUID, name: String, sequence: Long) extends Event
case class InventoryItemDeactivated(id: UUID, sequence: Long) extends Event
case class InventoryItemRenamed(id: UUID, newName: String, sequence: Long) extends Event
case class ItemsCheckedInToInventory(id: UUID, count: Int, sequence: Long) extends Event
case class ItemsRemovedFromInventory(id: UUID, count: Int, sequence: Long) extends Event