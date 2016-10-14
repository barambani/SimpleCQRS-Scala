package SimpleCqrsScala.CommandSide

import java.util.UUID

sealed trait Command extends Identified with Product with Serializable

//	Order
final case class CreateOrder(id: UUID, customerId: UUID, customerName: UUID) extends Command
final case class AddInventoryItemToOrder(id: UUID, inventoryItemId: UUID, quantity: Int) extends Command
final case class RemoveInventoryItemFromOrder(id: UUID, inventoryItemId: UUID, quantity: Int) extends Command
final case class AddShippingAddressToOrder(id: UUID, shippingAddress: String) extends Command
final case class PayForTheOrder(id: UUID) extends Command
final case class SubmitTheOrder(id: UUID) extends Command
final case class DispatchTheOrder(id: UUID) extends Command
final case class VoidTheOrder(id: UUID, reason: String) extends Command


//	Inventory Item
final case class CreateInventoryItem(id: UUID, name: String) extends Command
final case class DeactivateInventoryItem(id: UUID) extends Command
final case class RenameInventoryItem(id: UUID, newName: String) extends Command
final case class CheckInItemsToInventory(id: UUID, count: Int) extends Command
final case class RemoveItemsFromInventory(id: UUID, count: Int) extends Command