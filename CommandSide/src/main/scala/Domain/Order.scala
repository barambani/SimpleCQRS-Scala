package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._

object Order {
	def apply(history: List[Event]): Order = AggregateRoot.evolve(new Order, history)
}

class Order private (
	val id: UUID = new UUID(0, 0),
	val description: String = "",
	val shippingAddress: String = "",
	val isPayed: Boolean = false,
	val isSubmitted: Boolean = false,
	val items: Map[UUID, Int] = Map.empty,
	val version: Long = 0) extends Identity with Versioned {

	private def addItemsToOrder(itemId: UUID, quantity: Int): Map[UUID, Int] = ???
	private def removeItemsFromOrder(itemId: UUID, quantity: Int): Map[UUID, Int] = ???

	//	Domain logic
	private def theItemCanBeRemoved(itemId: UUID, quantity: Int): Boolean = items.getOrElse(itemId, 0) >= quantity
	private def canBeChanged: Boolean = !isSubmitted
	private def theShippingAddressIsValid: Boolean = !shippingAddress.isEmpty
	private def canBePayed: Boolean = !isPayed
	private def canBeSubmitted: Boolean = isPayed && theShippingAddressIsValid

	def getNewStateWhen(event: Event): Order =
		if(!canBeChanged) this // TODO: Error, no changes are permitted after submission
		else if(!hasTheCorrectId(event)) this // TODO: Error in this case
		else if(!isInSequence(event)) this // TODO: Error in this case
		else
			event match {
				case OrderCreated(newId, description, sequence) => 
					new Order(id = newId, description = description, version = sequence)
				
				case InventoryItemAddedToOrder(_, inventoryItemId, quantity, sequence) => 
					new Order(id, description, shippingAddress, isPayed, isSubmitted, addItemsToOrder(inventoryItemId, quantity), sequence)

				case InventoryItemRemovedFromOrder(_, inventoryItemId, quantity, sequence) => 
					new Order(id, description, shippingAddress, isPayed, isSubmitted, removeItemsFromOrder(inventoryItemId, quantity), sequence)

				case ShippingAddressAddedToOrder(_, shippingAddress, sequence) => 
					new Order(id, description, shippingAddress, isPayed, isSubmitted, items, sequence)
				
				case OrderPayed(_, sequence) => 
					if(canBePayed) new Order(id, description, shippingAddress, true, isSubmitted, items, sequence)
					else this // TODO: Error, cannot be payed twice

				case OrderSubmitted(_, sequence) =>
					if(canBeSubmitted) new Order(id, description, shippingAddress, isPayed, true, items, sequence)
					else this // TODO: Error, the order cannot be submitted
				
				case _ => this
			}
	
	def addInventoryItemToOrder(inventoryItemId: UUID, quantity: Int): List[Event] =
		InventoryItemAddedToOrder(id, inventoryItemId, quantity, nextStateVersion).asHistory

	def removeInventoryItemFromOrder(inventoryItemId: UUID, quantity: Int): List[Event] =
		if(theItemCanBeRemoved(inventoryItemId, quantity))
			InventoryItemRemovedFromOrder (
				id, 
				inventoryItemId, 
				quantity, 
				nextStateVersion
			)
			.asHistory
		else Nil // TODO: Error, not enough items to remove

	def addShippingAddressToOrder(shippingAddress: String): List[Event] =
		ShippingAddressAddedToOrder(id, shippingAddress, nextStateVersion).asHistory

	def PayForTheOrder: List[Event] = 
		if(canBePayed) OrderPayed(id, nextStateVersion).asHistory
		else Nil // TODO: Error, cannot be payed twice

	def submitTheOrder: List[Event] =
		if(canBeSubmitted) OrderSubmitted(id, nextStateVersion).asHistory
		else Nil // TODO: Error, the order cannot be submitted
}