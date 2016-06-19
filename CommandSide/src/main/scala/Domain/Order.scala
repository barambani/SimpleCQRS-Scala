package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainTypes._

object Order {

	import AggregateRoot._

	def apply(history: List[Event]): Order = evolve(new Order, history)

	//	Behavior
	def addInventoryItemToOrder(inventoryItemId: UUID, quantity: Int): OrderS =
		getNewState(ord => InventoryItemAddedToOrder(ord.id, inventoryItemId, quantity, ord.nextStateVersion).asHistory)

	def removeInventoryItemFromOrder(inventoryItemId: UUID, quantity: Int): OrderS =
		getNewState(
			ord => 
				if(ord.theItemCanBeRemoved(inventoryItemId, quantity))
					InventoryItemRemovedFromOrder (
						ord.id, 
						inventoryItemId, 
						quantity, 
						ord.nextStateVersion
					)
					.asHistory
				else Nil // TODO: Error, not enough items to remove
		)

	def addShippingAddressToOrder(shippingAddress: String): OrderS =
		getNewState(ord => ShippingAddressAddedToOrder(ord.id, shippingAddress, ord.nextStateVersion).asHistory)

	def payTheBalance: OrderS = 
		getNewState(
			ord => 	if(ord.canBePayed) OrderPayed(ord.id, ord.nextStateVersion).asHistory
					else Nil // TODO: Error, cannot be payed twice
		)

	def submit: OrderS =
		getNewState(
			ord => 	if(ord.canBeSubmitted) OrderSubmitted(ord.id, ord.nextStateVersion).asHistory
					else Nil // TODO: Error, the order cannot be submitted
		)
}

class Order private (
	val id: UUID = new UUID(0, 0),
	val description: String = "",
	val shippingAddress: String = "",
	val isPayed: Boolean = false,
	val isSubmitted: Boolean = false,
	val items: Map[UUID, Int] = Map.empty,
	val version: Long = 0) extends Identity with Versioned {

	private def addOrUpdateItem: UUID => (Int => Int) => Map[UUID, Int] = MapOps.updateItem(items) _

	private def getItemsAfterAdditionOf(itemId: UUID, quantity: Int): Map[UUID, Int] =
		if(items contains itemId) addOrUpdateItem(itemId)(ei => ei + quantity)
		else items + (itemId -> quantity)

	private def getItemsAfterRemovalOf(itemId: UUID, quantity: Int): Map[UUID, Int] =
		if(items contains itemId) addOrUpdateItem(itemId)(ei => ei - quantity)
		else items // TODO: May be error? trying to remove unexisting item

	//	Domain logic
	private def theItemCanBeRemoved(itemId: UUID, quantity: Int): Boolean = (items getOrElse (itemId, 0)) >= quantity
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
					new Order(id, description, shippingAddress, isPayed, isSubmitted, getItemsAfterAdditionOf(inventoryItemId, quantity), sequence)

				case InventoryItemRemovedFromOrder(_, inventoryItemId, quantity, sequence) => 
					if(theItemCanBeRemoved(inventoryItemId, quantity))
						new Order(id, description, shippingAddress, isPayed, isSubmitted, getItemsAfterRemovalOf(inventoryItemId, quantity), sequence)
					else 
						this // TODO: Error, not enough items to remove

				case ShippingAddressAddedToOrder(_, shippingAddress, sequence) => 
					new Order(id, description, shippingAddress, isPayed, isSubmitted, items, sequence)
				
				case OrderPayed(_, sequence) => 
					if(canBePayed) new Order(id, description, shippingAddress, true, isSubmitted, items, sequence)
					else this // TODO: Error, cannot be payed twice

				case OrderSubmitted(_, sequence) =>
					if(canBeSubmitted) new Order(id, description, shippingAddress, isPayed, true, items, sequence)
					else this // TODO: Error, the order cannot be submitted
				
				case _ => this // TODO: log event ignored with event details
			}
}