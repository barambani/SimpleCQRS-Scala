package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import scala.language.implicitConversions

sealed trait ErrorMessage extends Product with Serializable

final case class InventoryItemNameNotValid(itemId: UUID, name: Option[String], requestedName: String) extends ErrorMessage
final case class NotEnoughItemsInStock(itemId: UUID, name: String, requestedCount: Int) extends ErrorMessage

final case class ShippingAddressNotValid(orderId: UUID, description: String, address: String) extends ErrorMessage
final case class OrderClosed(orderId: UUID, description: String) extends ErrorMessage
final case class NotEnoughItemsInTheOrder(orderId: UUID, description: String, itemId: UUID, requestedCount: Int) extends ErrorMessage
final case class OrderAlreadyPayed(orderId: UUID, description: String) extends ErrorMessage
final case class OrderNotComplete(orderId: UUID, description: String) extends ErrorMessage