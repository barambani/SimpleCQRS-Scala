package Commands

import java.util.UUID

sealed trait Command
case class CreateInventoryItem(id: UUID, name: String) extends Command
case class DeactivateInventoryItem(id: UUID) extends Command
case class RenameInventoryItem(id: UUID, newName: String) extends Command
case class CheckInItemsToInventory(id: UUID, count: Int) extends Command
case class RemoveItemsFromInventory(id: UUID, count: Int) extends Command