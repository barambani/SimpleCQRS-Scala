package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainStates._
import OrderItems._

object OrderOps {

	import AggregateRoot._
	import EventOps._

	//	Behavior
	def addInventoryItemToOrder(inventoryItemId: UUID, quantity: Int): OrderS =
		getNewState(ord => InventoryItemAddedToOrder(ord.id, inventoryItemId, quantity, nextStateVersion(ord)).asHistory)

	def removeInventoryItemFromOrder(inventoryItemId: UUID, quantity: Int): OrderS = getNewState(
		ord => 
			if(canRemoveTheItem(ord.items, inventoryItemId, quantity))
				InventoryItemRemovedFromOrder (
					ord.id, 
					inventoryItemId, 
					quantity, 
					nextStateVersion(ord)
				).asHistory
			else Nil // TODO: Error, not enough items to remove
	)

	def addShippingAddressToOrder(shippingAddress: String): OrderS =
		getNewState(ord => ShippingAddressAddedToOrder(ord.id, shippingAddress, nextStateVersion(ord)).asHistory)

	def payTheBalance: OrderS = getNewState(
		ord => 	if(ord.canBePayed) OrderPayed(ord.id, nextStateVersion(ord)).asHistory
				else Nil // TODO: Error, cannot be payed twice
	)

	def submit: OrderS = getNewState(
		ord => 	if(ord.canBeSubmitted) OrderSubmitted(ord.id, nextStateVersion(ord)).asHistory
				else Nil // TODO: Error, the order cannot be submitted
	)

	lazy val newState: Order => Event => Order = 
		aggregate => event =>
			if(!aggregate.canBeChanged) aggregate // TODO: Error, no changes are permitted after submission
			else if(!hasACorrectIdCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequenceCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else
				event match {
					case OrderCreated(newId, description, sequence) => 
						new Order(id = newId, description = description, version = sequence)
					case InventoryItemAddedToOrder(_, inventoryItemId, quantity, sequence) => 
						new Order(
							aggregate.id, 
							aggregate.description, 
							aggregate.shippingAddress, 
							aggregate.isPayed, 
							aggregate.isSubmitted, 
							addToItems(aggregate.items, inventoryItemId, quantity), 
							sequence
						)
					case InventoryItemRemovedFromOrder(_, inventoryItemId, quantity, sequence) => 
						if(canRemoveTheItem(aggregate.items, inventoryItemId, quantity))
							new Order(
								aggregate.id, 
								aggregate.description, 
								aggregate.shippingAddress, 
								aggregate.isPayed, 
								aggregate.isSubmitted, 
								removeFromItems(aggregate.items, inventoryItemId, quantity), 
								sequence
							)
						else 
							aggregate // TODO: Error, not enough items to remove

					case ShippingAddressAddedToOrder(_, shippingAddress, sequence) => 
						new Order(
							aggregate.id,
							aggregate.description, 
							shippingAddress, 
							aggregate.isPayed, 
							aggregate.isSubmitted, 
							aggregate.items, 
							sequence
						)
					case OrderPayed(_, sequence) => 
						if(aggregate.canBePayed) 
							new Order(
								aggregate.id, 
								aggregate.description,
								aggregate.shippingAddress, 
								true, 
								aggregate.isSubmitted, 
								aggregate.items, 
								sequence
							)
						else aggregate // TODO: Error, cannot be payed twice

					case OrderSubmitted(_, sequence) =>
						if(aggregate.canBeSubmitted) 
							new Order(
								aggregate.id, 
								aggregate.description, 
								aggregate.shippingAddress, 
								aggregate.isPayed, 
								true, 
								aggregate.items, 
								sequence
							)
						else aggregate // TODO: Error, the order cannot be submitted
					
					case _ => aggregate // TODO: log event ignored with event details
				}

	object Order {
		import AggregateRoot._
		
		def apply(history: List[Event]): Order = evolve(new Order, history)
	}
	case class Order private[OrderOps] (
		val id: UUID = new UUID(0, 0),
		val description: String = "",
		val shippingAddress: String = "",
		val isPayed: Boolean = false,
		val isSubmitted: Boolean = false,
		val items: OrderItems = OrderItems.empty,
		val version: Long = 0) extends Identity with Versioned {

		//	Domain logic
		lazy val canBeChanged: Boolean = !isSubmitted
		lazy val theShippingAddressIsValid: Boolean = !shippingAddress.isEmpty
		lazy val canBePayed: Boolean = !isPayed
		lazy val canBeSubmitted: Boolean = isPayed && theShippingAddressIsValid
	}
}
