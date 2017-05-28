package SimpleCqrsScala.CommandSide.Validation

import SimpleCqrsScala.CommandSide.Domain.{Order, Open}
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.Errors._

import java.util.UUID

trait OrderValidation {

	def canBeChanged(ord: Order): Validated[Order] = 
		ord.status match {
			case Open	=> succeeded(ord)
			case _	 	=> failedWith(OrderClosed(ord.id, ord.description))
		}

	def shippingAddressValid(ord: Order)(shippingAddress: String): Validated[String] = 
		shippingAddress.isEmpty match {
			case true 	=> failedWith(ShippingAddressNotValid(ord.id, ord.description, shippingAddress))
			case false 	=> succeeded(shippingAddress)
		}

	def hasEnoughItems(ord: Order)(itemId: UUID, quantity: Int): Validated[(UUID, Int)] = 
		(ord.items get itemId).fold(false){ _ >= quantity } match {
			case true 	=> succeeded((itemId, quantity))
			case false 	=> failedWith(NotEnoughItemsInTheOrder(ord.id, ord.description, itemId, quantity))
		}

	def isPaymentValid(ord: Order): Validated[Order] = 
		ord.isPayed match {
			case true 	=> succeeded(ord)
			case false 	=> failedWith(OrderPaymentIsNotValid(ord.id, ord.description))
		}

	def canBePayed(ord: Order): Validated[Order] = 
		ord.isPayed match {
			case false 	=> succeeded(ord)
			case true 	=> failedWith(OrderAlreadyPayed(ord.id, ord.description))
		}

	def containsItems(ord: Order): Validated[Order] =
		ord.items.isEmpty match {
			case false 	=> succeeded(ord)
			case true 	=> failedWith(OrderContainsNoItems(ord.id, ord.description))
		}
}