package SimpleCqrsScala.CommandSide.Services

import SimpleCqrsScala.CommandSide.Domain.Order
import SimpleCqrsScala.CommandSide.Domain.Open
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Errors._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import SimpleCqrsScala.CommandSide.Domain.DomainState.EitherTransition._
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._
import java.util.UUID
import scalaz.Scalaz._

trait OrderService {

	def createOrderFor(id: UUID, customerId: UUID, customerName: String): EitherTransition[Order] =
		liftEvents{
			OrderCreated(id, s"Order for $customerName (id: $customerId)", 1) :: Nil
		}

	def addInventoryItemToOrder(itemId: UUID, quantity: Int): EitherTransition[Order] =
		liftValidatedF {
			ord => canBeChanged(ord) map { 
				_ => InventoryItemAddedToOrder(ord.id, itemId, quantity, ord.expectedNextVersion) :: Nil
			}
		}

	def removeInventoryItemFromOrder(itemId: UUID, quantity: Int): EitherTransition[Order] = 
		liftValidatedF {
			ord => (canBeChanged(ord) |@| hasEnoughItems(ord)(itemId, quantity)) { 
				(_, _) => InventoryItemRemovedFromOrder(ord.id, itemId, quantity, ord.expectedNextVersion) :: Nil
			}
		}

	def addShippingAddressToOrder(address: String): EitherTransition[Order] =
		liftValidatedF {
			ord => (canBeChanged(ord) |@| shippingAddressValid(ord)(address)) { 
				(_, _) => ShippingAddressAddedToOrder(ord.id, address, ord.expectedNextVersion) :: Nil
			}
		}

	def payTheBalance: EitherTransition[Order] =
		liftValidatedF {
			ord => (canBeChanged(ord) |@| canBePayed(ord)) { 
				(_, _) => OrderPayed(ord.id, ord.expectedNextVersion) :: Nil
			}
		}

	def submit: EitherTransition[Order] = 
		liftValidatedF {
			ord => (canBeChanged(ord) |@| containsItems(ord) |@| isPaymentValid(ord)) { 
				(_, _, _) => OrderSubmitted(ord.id, ord.expectedNextVersion) :: Nil
			}
		}


	//	Validation
	private def canBeChanged(ord: Order): Validated[Order] = 
		ord.status match {
			case Open	=> succeeded(ord)
			case _	 	=> failedWith(OrderClosed(ord.id, ord.description))
		}

	private def shippingAddressValid(ord: Order)(shippingAddress: String): Validated[String] = 
		shippingAddress.isEmpty match {
			case true 	=> failedWith(ShippingAddressNotValid(ord.id, ord.description, shippingAddress))
			case false 	=> succeeded(shippingAddress)
		}

	private def hasEnoughItems(ord: Order)(itemId: UUID, quantity: Int): Validated[(UUID, Int)] = 
		(ord.items get itemId).fold(false){ _ >= quantity } match {
			case true 	=> succeeded((itemId, quantity))
			case false 	=> failedWith(NotEnoughItemsInTheOrder(ord.id, ord.description, itemId, quantity))
		}

	private def isPaymentValid(ord: Order): Validated[Order] = 
		ord.isPayed match {
			case true 	=> succeeded(ord)
			case false 	=> failedWith(OrderPaymentIsNotValid(ord.id, ord.description))
		}

	private def canBePayed(ord: Order): Validated[Order] = 
		ord.isPayed match {
			case false 	=> succeeded(ord)
			case true 	=> failedWith(OrderAlreadyPayed(ord.id, ord.description))
		}

	private def containsItems(ord: Order): Validated[Order] =
		ord.items.isEmpty match {
			case false 	=> succeeded(ord)
			case true 	=> failedWith(OrderContainsNoItems(ord.id, ord.description))
		}
}