package SimpleCqrsScala.CommandSide

import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Events.Event._
import SimpleCqrsScala.CommandSide.Domain.Errors._
import scalaz.NonEmptyList
import scalaz.syntax.foldable._ 

trait Print[A] {
  def stringFor(a: A): String
}

object Print {
  def apply[A](implicit INST: Print[A]): Print[A] = INST

  implicit class PrintSyntax[A](a: A) {
    def print(implicit SH: Print[A]): String = SH stringFor a
  }
}

trait OrderEventsPrint {

  implicit object OrderCreatedPrint extends Print[OrderCreated] {
    def stringFor(x: OrderCreated): String = 
      s"seq: ${x.sequence} - Order ${x.description} created (id: ${x.id})"
  }
  
  implicit object InventoryItemAddedToOrderPrint extends Print[InventoryItemAddedToOrder] {
    def stringFor(x: InventoryItemAddedToOrder): String = 
      s"seq: ${x.sequence} - ${x.quantity} of inventory item ${x.inventoryItemId} added to order (id: ${x.id})"
  }
  
  implicit object InventoryItemRemovedFromOrderPrint extends Print[InventoryItemRemovedFromOrder] {
    def stringFor(x: InventoryItemRemovedFromOrder): String = 
      s"seq: ${x.sequence} - ${x.quantity} of inventory item ${x.inventoryItemId} removed from order (id: ${x.id})"
  }
  
  implicit object ShippingAddressAddedToOrderPrint extends Print[ShippingAddressAddedToOrder] {
    def stringFor(x: ShippingAddressAddedToOrder): String = 
      s"seq: ${x.sequence} - address: '${x.shippingAddress}' added order (id: ${x.id})"
  }
  
  implicit object OrderPayedPrint extends Print[OrderPayed] {
    def stringFor(x: OrderPayed): String = s"seq: ${x.sequence} - order payed (id: ${x.id})"
  }
  
  implicit object OrderSubmittedPrint extends Print[OrderSubmitted] {
    def stringFor(x: OrderSubmitted): String = s"seq: ${x.sequence} - order submitted (id: ${x.id})"
  }
}

trait InventoryItemEventsPrint {

  implicit object InventoryItemCreatedPrint extends Print[InventoryItemCreated] {
    def stringFor(x: InventoryItemCreated): String = s"seq: ${x.sequence} - Item ${x.name} created (id: ${x.id})"
  }
  
  implicit object InventoryItemDeactivatedPrint extends Print[InventoryItemDeactivated] {
    def stringFor(x: InventoryItemDeactivated): String = s"seq: ${x.sequence} - Item deactivated (id: ${x.id})"
  }
  
  implicit object InventoryItemRenamedPrint extends Print[InventoryItemRenamed] {
    def stringFor(x: InventoryItemRenamed): String = s"seq: ${x.sequence} - Item renamed to ${x.newName} (id: ${x.id})"
  }
  
  implicit object ItemsCheckedInToInventoryPrint extends Print[ItemsCheckedInToInventory] {
    def stringFor(x: ItemsCheckedInToInventory): String = s"seq: ${x.sequence} - Check-in of ${x.count} item/s (id: ${x.id})"
  }
  
  implicit object ItemsRemovedFromInventoryPrint extends Print[ItemsRemovedFromInventory] {
    def stringFor(x: ItemsRemovedFromInventory): String = s"seq: ${x.sequence} - Removed ${x.count} item/s (id: ${x.id})"
  }
}

trait ErrorsPrint {

  implicit object ImpossibleToCreate extends Print[ImpossibleToCreate] {
    def stringFor(x: ImpossibleToCreate): String = s"Impossible to create an item starting from a non empty state. Details ${x.id}"
  }
  
  implicit object ErrorFromThrowablePrint extends Print[ErrorFromThrowable] {
    def stringFor(x: ErrorFromThrowable): String = s"Error from throwable with message ${x.message}"
  }
  
  implicit object InventoryItemNameNotValidPrint extends Print[InventoryItemNameNotValid] {
    def stringFor(x: InventoryItemNameNotValid): String = s"the name '${x.requestedName}' proposed for the item ${x.name} (id: ${x.itemId}) is not valid"
  }
  
  implicit object NotEnoughItemsInStockPrint extends Print[NotEnoughItemsInStock] {
    def stringFor(x: NotEnoughItemsInStock): String = s"the inventory doesn't have ${x.requestedCount} items of ${x.name} (id: ${x.itemId})"
  }
  
  implicit object OrderClosedPrint extends Print[OrderClosed] {
    def stringFor(x: OrderClosed): String = s"the order ${x.description} (id: ${x.orderId}) cannot be changed as it has been closed"
  }
  
  implicit object NotEnoughItemsInTheOrderPrint extends Print[NotEnoughItemsInTheOrder] {
    def stringFor(x: NotEnoughItemsInTheOrder): String =
      s"impossible to remove ${x.requestedCount} of items (of id: ${x.itemId}) from the order ${x.description} (id: ${x.orderId})"
  }
  
  implicit object ShippingAddressNotValidPrint extends Print[ShippingAddressNotValid] {
    def stringFor(x: ShippingAddressNotValid): String =
      s"the shipping address '${x.address}' proposed for the order '${x.description}' (id: ${x.orderId}) is not valid"
  }
  
  implicit object OrderAlreadyPayedPrint extends Print[OrderAlreadyPayed] {
    def stringFor(x: OrderAlreadyPayed): String = s"the order ${x.description} (id: ${x.orderId}) is already payed and can't be payed again"
  }
  
  implicit object OrderNotCompletePrint extends Print[OrderPaymentIsNotValid] {
    def stringFor(x: OrderPaymentIsNotValid): String = 
      s"the payment for order ${x.description} (id: ${x.orderId}) is not valid"
  }
  
  implicit object OrderContainsNoItemsPrint extends Print[OrderContainsNoItems] {
    def stringFor(x: OrderContainsNoItems): String = 
      s"the order ${x.description} (id: ${x.orderId}) contains no items"
  }

  implicit object ErrorMessagePrint extends Print[ErrorMessage] {
    def stringFor(x: ErrorMessage): String = x match {
      case e: ImpossibleToCreate        => Print[ImpossibleToCreate].stringFor(e)
      case e: ErrorFromThrowable 	=> Print[ErrorFromThrowable].stringFor(e)
      case e: InventoryItemNameNotValid => Print[InventoryItemNameNotValid].stringFor(e)
      case e: NotEnoughItemsInStock     => Print[NotEnoughItemsInStock].stringFor(e)
      case e: ShippingAddressNotValid 	=> Print[ShippingAddressNotValid].stringFor(e)
      case e: OrderClosed   	        => Print[OrderClosed].stringFor(e)
      case e: NotEnoughItemsInTheOrder 	=> Print[NotEnoughItemsInTheOrder].stringFor(e)
      case e: OrderAlreadyPayed         => Print[OrderAlreadyPayed].stringFor(e)
      case e: OrderPaymentIsNotValid  	=> Print[OrderPaymentIsNotValid].stringFor(e)
      case e: OrderContainsNoItems 	=> Print[OrderContainsNoItems].stringFor(e)
    }
  }

  implicit object NonEmptyListErrorPrint extends Print[NonEmptyList[ErrorMessage]] {
    def stringFor(xs: NonEmptyList[ErrorMessage]): String =
      (xs map Print[ErrorMessage].stringFor).foldLeft("") { (r, n) => s"$r $n" }.trim
  }
}

