package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds
import OrderItems._

import AggregateRoot._
import Event._

sealed trait OrderStatus extends Product with Serializable
final case object Open extends OrderStatus
final case object Submitted extends OrderStatus
final case object WaitingForItems extends OrderStatus
final case object AllItemsAvailable extends OrderStatus
final case object Dispatched extends OrderStatus
final case object Voided extends OrderStatus

@Lenses case class Order private[Order] (
	id: UUID = new UUID(0, 0),
	description: String = "",
	shippingAddress: String = "",
	isPayed: Boolean = false,
	items: OrderItems = OrderItems.empty,
	status: OrderStatus = Open,
	version: Long = 0) extends Identity with Versioned {

	//	Domain logic
	lazy val canBeChanged: Boolean = status == Open
	lazy val theShippingAddressIsValid: Boolean = !shippingAddress.isEmpty
	lazy val canBePayed: Boolean = !isPayed
	lazy val canBeSubmitted: Boolean = isPayed && theShippingAddressIsValid
	lazy val canBeDispatched: Boolean = status == AllItemsAvailable && isPayed && theShippingAddressIsValid
	lazy val canBeVoided: Boolean = status != Dispatched
}

object Order {
	
	//	Factories
	def apply(history: Event*): Order = apply(history.toList)
	def apply(history: List[Event]): Order = evolve(new Order, history)

	//	Commands
	lazy val addInventoryItemToOrder: UUID => Int => StateTransition[Order] =
		inventoryItemId => quantity => 
			newStateTransition(
				ord =>	if(ord.canBeChanged) InventoryItemAddedToOrder(ord.id, inventoryItemId, quantity, ord.expectedNextVersion) :: Nil
						else Nil // TODO: Error, cannot be changed
			)

	lazy val removeInventoryItemFromOrder: UUID => Int => StateTransition[Order] = 
		inventoryItemId => quantity => 
			newStateTransition(
				ord =>	if(ord.canBeChanged)
							if(canRemoveTheItem(ord.items, inventoryItemId, quantity)) 
								InventoryItemRemovedFromOrder(ord.id, inventoryItemId, quantity, ord.expectedNextVersion) :: Nil
							else
								Nil // TODO: Error, not enough items to remove
						else
							Nil // TODO: Error, cannot be changed
			)

	lazy val addShippingAddressToOrder: String => StateTransition[Order] =
		shippingAddress => newStateTransition(
			ord =>	if(ord.canBeChanged) ShippingAddressAddedToOrder(ord.id, shippingAddress, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, cannot be changed
		)

	lazy val payTheBalance: () => StateTransition[Order] = 
		() => newStateTransition(
			ord => 	if(ord.canBePayed) OrderPayed(ord.id, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, cannot be payed twice
		)

	lazy val submit: () => StateTransition[Order] = 
		() => newStateTransition(
			ord => 	if(ord.canBeSubmitted) OrderSubmitted(ord.id, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, the order cannot be submitted
		)

	//	Aggregate Evolution
	lazy val newState: Order => Event => Order = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case OrderCreated(newId, description, sequence) => 
					new Order(id = newId, description = description, version = sequence)

				case InventoryItemAddedToOrder(_, inventoryItemId, quantity, sequence) => 
					withItems(addToItems(aggregate.items, inventoryItemId, quantity))(sequence)(aggregate)
					
				case InventoryItemRemovedFromOrder(_, inventoryItemId, quantity, sequence) => 
					withItems(removeFromItems(aggregate.items, inventoryItemId, quantity))(sequence)(aggregate)
					
				case ShippingAddressAddedToOrder(_, shippingAddress, sequence) => 
					withAddress(shippingAddress)(sequence)(aggregate)

				case OrderPayed(_, sequence) =>
					afterPayed(sequence)(aggregate)

				case OrderSubmitted(_, sequence) =>
					afterSubmitted(sequence)(aggregate)
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	//	Lenses
	private lazy val withAddress: String => Long => Order => Order =
		addr => ver => Order.version.set(ver) compose Order.shippingAddress.set(addr)

	private lazy val afterPayed: Long => Order => Order =
		ver => Order.version.set(ver) compose Order.isPayed.set(true)

	private lazy val afterSubmitted: Long => Order => Order =
		ver => Order.version.set(ver) compose Order.status.set(Submitted)

	private lazy val withItems: OrderItems => Long => Order => Order =
		is => ver => Order.version.set(ver) compose Order.items.set(is)
}
