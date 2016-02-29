package Events

import java.util.UUID

sealed trait Event {
	override def toString: String = this match {
		case InventoryItemCreated(id, name) => "Item " + name + " created (id:" + id.toString() + ")"
		case InventoryItemDeactivated(id) => "Item deactivated (id:" + id.toString() + ")"
		case InventoryItemRenamed(id, newName) => "Item renamed to " + newName + " (id:" + id.toString() + ")"
		case ItemsCheckedInToInventory(id, count) => "Check-in " + count.toString() + " of item (id:" + id.toString() + ")"
		case ItemsRemovedFromInventory(id, count) => "Removed " + count.toString() + " of item (id:" + id.toString() + ")"
		case _ => ""
	} 
}

case class InventoryItemCreated(id: UUID, name: String) extends Event
case class InventoryItemDeactivated(id: UUID) extends Event
case class InventoryItemRenamed(id: UUID, newName: String) extends Event
case class ItemsCheckedInToInventory(id: UUID, count: Int) extends Event
case class ItemsRemovedFromInventory(id: UUID, count: Int) extends Event