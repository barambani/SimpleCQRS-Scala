package Events

import java.util.UUID

sealed trait Event
case class InventoryItemCreated(id: UUID, name: String) extends Event
case class InventoryItemDeactivated(id: UUID) extends Event
case class InventoryItemRenamed(id: UUID, newName: String) extends Event
case class ItemsCheckedInToInventory(id: UUID, count: Int) extends Event
case class ItemsRemovedFromInventory(id: UUID, count: Int) extends Event