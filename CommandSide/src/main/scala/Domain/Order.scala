package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds
import OrderItems._

import AggregateRoot._
import EventOps._

@Lenses case class Order private[Order] (
	id: UUID = new UUID(0, 0),
	description: String = "",
	shippingAddress: String = "",
	isPayed: Boolean = false,
	isSubmitted: Boolean = false,
	items: OrderItems = OrderItems.empty,
	version: Long = 0) extends Identity with Versioned {

	//	Domain logic
	lazy val canBeChanged: Boolean = !isSubmitted
	lazy val theShippingAddressIsValid: Boolean = !shippingAddress.isEmpty
	lazy val canBePayed: Boolean = !isPayed
	lazy val canBeSubmitted: Boolean = isPayed && theShippingAddressIsValid
}

object Order {
	
	def apply(history: Event*): Order = apply(history.toList)
	def apply(history: List[Event]): Order = evolve(new Order, history)

	//	Behavior
	def addInventoryItemToOrder(inventoryItemId: UUID, quantity: Int): StateTransition[Order] =
		newStateTransition(ord => InventoryItemAddedToOrder(ord.id, inventoryItemId, quantity, ord.expectedNextVersion) :: Nil)

	def removeInventoryItemFromOrder(inventoryItemId: UUID, quantity: Int): StateTransition[Order] = 
		newStateTransition(
			ord => 
				if(canRemoveTheItem(ord.items, inventoryItemId, quantity)) 
					InventoryItemRemovedFromOrder(ord.id, inventoryItemId, quantity, ord.expectedNextVersion) :: Nil
				else
					Nil // TODO: Error, not enough items to remove
		)

	def addShippingAddressToOrder(shippingAddress: String): StateTransition[Order] =
		newStateTransition(ord => ShippingAddressAddedToOrder(ord.id, shippingAddress, ord.expectedNextVersion) :: Nil)

	def payTheBalance: StateTransition[Order] = 
		newStateTransition(
			ord => 	if(ord.canBePayed) OrderPayed(ord.id, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, cannot be payed twice
		)

	def submit: StateTransition[Order] = 
		newStateTransition(
			ord => 	if(ord.canBeSubmitted) OrderSubmitted(ord.id, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, the order cannot be submitted
		)

	lazy val newState: Order => Event => Order = 
		aggregate => event =>
			if(!aggregate.canBeChanged) aggregate // TODO: Error, no changes are permitted after submission
			else if(!hasACorrectIdCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequenceCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case OrderCreated(newId, description, sequence) => 
					new Order(id = newId, description = description, version = sequence)

				case InventoryItemAddedToOrder(_, inventoryItemId, quantity, sequence) => 
					getNewWithItems(addToItems(aggregate.items, inventoryItemId, quantity), sequence)(aggregate)
					
				case InventoryItemRemovedFromOrder(_, inventoryItemId, quantity, sequence) => 
					if(canRemoveTheItem(aggregate.items, inventoryItemId, quantity))
						getNewWithItems(removeFromItems(aggregate.items, inventoryItemId, quantity), sequence)(aggregate)
					else 
						aggregate // TODO: Error, not enough items to remove

				case ShippingAddressAddedToOrder(_, shippingAddress, sequence) => 
					getNewWithAddress(shippingAddress, sequence)(aggregate)

				case OrderPayed(_, sequence) =>
					if(aggregate.canBePayed) getNewPayed(sequence)(aggregate)
					else aggregate // TODO: Error, cannot be payed twice

				case OrderSubmitted(_, sequence) =>
					if(aggregate.canBeSubmitted) getNewSubmitted(sequence)(aggregate)
					else aggregate // TODO: Error, the order cannot be submitted
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	private lazy val getNewWithAddress: (String, Long) => Order => Order =
		(addr, ver) => Order.version.set(ver) compose Order.shippingAddress.set(addr)

	private lazy val getNewPayed: Long => Order => Order =
		ver => Order.version.set(ver) compose Order.isPayed.set(true)

	private lazy val getNewSubmitted: Long => Order => Order =
		ver => Order.version.set(ver) compose Order.isSubmitted.set(true)

	private lazy val getNewWithItems: (OrderItems, Long) => Order => Order =
		(is, ver) => Order.version.set(ver) compose Order.items.set(is)
}
