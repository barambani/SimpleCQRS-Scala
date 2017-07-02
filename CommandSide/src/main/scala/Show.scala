package SimpleCqrsScala.CommandSide

import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Events.Event._
import SimpleCqrsScala.CommandSide.Domain.Errors._
import scalaz.NonEmptyList
import scalaz.syntax.foldable._ 

trait Show[A] {
	def stringFor(a: A): String
}
object Show {
	def apply[A](implicit instance: Show[A]): Show[A] = instance
}

trait OrderEventsShow {

	implicit object OrderCreatedShow extends Show[OrderCreated] {
		def stringFor(x: OrderCreated): String = 
			s"seq: ${x.sequence} - Order ${x.description} created (id: ${x.id})"
	}
	implicit object InventoryItemAddedToOrderShow extends Show[InventoryItemAddedToOrder] {
		def stringFor(x: InventoryItemAddedToOrder): String = 
			s"seq: ${x.sequence} - ${x.quantity} of inventory item ${x.inventoryItemId} added to order (id: ${x.id})"
	}
	implicit object InventoryItemRemovedFromOrderShow extends Show[InventoryItemRemovedFromOrder] {
		def stringFor(x: InventoryItemRemovedFromOrder): String = 
			s"seq: ${x.sequence} - ${x.quantity} of inventory item ${x.inventoryItemId} removed from order (id: ${x.id})"
	}
	implicit object ShippingAddressAddedToOrderShow extends Show[ShippingAddressAddedToOrder] {
		def stringFor(x: ShippingAddressAddedToOrder): String = 
			s"seq: ${x.sequence} - address: '${x.shippingAddress}' added order (id: ${x.id})"
	}
	implicit object OrderPayedShow extends Show[OrderPayed] {
		def stringFor(x: OrderPayed): String = s"seq: ${x.sequence} - order payed (id: ${x.id})"
	}
	implicit object OrderSubmittedShow extends Show[OrderSubmitted] {
		def stringFor(x: OrderSubmitted): String = s"seq: ${x.sequence} - order submitted (id: ${x.id})"
	}
}

trait InventoryItemEventsShow {

	implicit object InventoryItemCreatedShow extends Show[InventoryItemCreated] {
		def stringFor(x: InventoryItemCreated): String = s"seq: ${x.sequence} - Item ${x.name} created (id: ${x.id})"
	}
	implicit object InventoryItemDeactivatedShow extends Show[InventoryItemDeactivated] {
		def stringFor(x: InventoryItemDeactivated): String = s"seq: ${x.sequence} - Item deactivated (id: ${x.id})"
	}
	implicit object InventoryItemRenamedShow extends Show[InventoryItemRenamed] {
		def stringFor(x: InventoryItemRenamed): String = s"seq: ${x.sequence} - Item renamed to ${x.newName} (id: ${x.id})"
	}
	implicit object ItemsCheckedInToInventoryShow extends Show[ItemsCheckedInToInventory] {
		def stringFor(x: ItemsCheckedInToInventory): String = s"seq: ${x.sequence} - Check-in of ${x.count} item/s (id: ${x.id})"
	}
	implicit object ItemsRemovedFromInventoryShow extends Show[ItemsRemovedFromInventory] {
		def stringFor(x: ItemsRemovedFromInventory): String = s"seq: ${x.sequence} - Removed ${x.count} item/s (id: ${x.id})"
	}
}

trait ErrorsShow {

	implicit object ErrorFromThrowableShow extends Show[ErrorFromThrowable] {
		def stringFor(x: ErrorFromThrowable): String = s"Error from throwable with message ${x.message}"
	}
	implicit object InventoryItemNameNotValidShow extends Show[InventoryItemNameNotValid] {
		def stringFor(x: InventoryItemNameNotValid): String = s"the name '${x.requestedName}' proposed for the item ${x.name} (id: ${x.itemId}) is not valid"
	}
	implicit object NotEnoughItemsInStockShow extends Show[NotEnoughItemsInStock] {
		def stringFor(x: NotEnoughItemsInStock): String = s"the inventory doesn't have ${x.requestedCount} items of ${x.name} (id: ${x.itemId})"
	}
	implicit object OrderClosedShow extends Show[OrderClosed] {
		def stringFor(x: OrderClosed): String = s"the order ${x.description} (id: ${x.orderId}) cannot be changed as it has been closed"
	}
	implicit object NotEnoughItemsInTheOrderShow extends Show[NotEnoughItemsInTheOrder] {
		def stringFor(x: NotEnoughItemsInTheOrder): String =
			s"impossible to remove ${x.requestedCount} of items (of id: ${x.itemId}) from the order ${x.description} (id: ${x.orderId})"
	}
	implicit object ShippingAddressNotValidShow extends Show[ShippingAddressNotValid] {
		def stringFor(x: ShippingAddressNotValid): String =
			s"the shipping address '${x.address}' proposed for the order '${x.description}' (id: ${x.orderId}) is not valid"
	}
	implicit object OrderAlreadyPayedShow extends Show[OrderAlreadyPayed] {
		def stringFor(x: OrderAlreadyPayed): String = s"the order ${x.description} (id: ${x.orderId}) is already payed and can't be payed again"
	}
	implicit object OrderNotCompleteShow extends Show[OrderPaymentIsNotValid] {
		def stringFor(x: OrderPaymentIsNotValid): String = 
			s"the payment for order ${x.description} (id: ${x.orderId}) is not valid"
	}
	implicit object OrderContainsNoItemsShow extends Show[OrderContainsNoItems] {
		def stringFor(x: OrderContainsNoItems): String = 
			s"the order ${x.description} (id: ${x.orderId}) contains no items"
	}

	implicit object ErrorMessageShow extends Show[ErrorMessage] {
		def stringFor(x: ErrorMessage): String = x match {
			case e: ErrorFromThrowable 			=> Show[ErrorFromThrowable].stringFor(e)
			case e: InventoryItemNameNotValid 	=> Show[InventoryItemNameNotValid].stringFor(e)
			case e: NotEnoughItemsInStock 		=> Show[NotEnoughItemsInStock].stringFor(e)
			case e: ShippingAddressNotValid 	=> Show[ShippingAddressNotValid].stringFor(e)
			case e: OrderClosed 				=> Show[OrderClosed].stringFor(e)
			case e: NotEnoughItemsInTheOrder 	=> Show[NotEnoughItemsInTheOrder].stringFor(e)
			case e: OrderAlreadyPayed 			=> Show[OrderAlreadyPayed].stringFor(e)
			case e: OrderPaymentIsNotValid 		=> Show[OrderPaymentIsNotValid].stringFor(e)
			case e: OrderContainsNoItems 		=> Show[OrderContainsNoItems].stringFor(e)
		}
	}

	implicit object NonEmptyListErrorShow extends Show[NonEmptyList[ErrorMessage]] {
		def stringFor(xs: NonEmptyList[ErrorMessage]): String =
			(xs map Show[ErrorMessage].stringFor).foldLeft("") { (r, n) => s"$r $n" }.trim
	}
}

