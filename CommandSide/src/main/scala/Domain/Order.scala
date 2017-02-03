package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds

import monocle.{Lens, Optional}
import monocle.macros.GenLens
import monocle.function.all.at
import monocle.std.map._
import scalaz.Scalaz._

import OrderItems._

object OrderItems {	
	type OrderItems = Map[UUID, Int]
	lazy val empty: OrderItems = Map.empty
}

sealed trait OrderStatus extends Product with Serializable
final case object Open extends OrderStatus
final case object Submitted extends OrderStatus
final case object WaitingForItems extends OrderStatus
final case object AllItemsAvailable extends OrderStatus
final case object Dispatched extends OrderStatus
final case object Voided extends OrderStatus

@Lenses final case class Order private (
	id: UUID,
	description: String,
	shippingAddress: String,
	isPayed: Boolean,
	items: OrderItems,
	status: OrderStatus,
	version: Long) extends Identity with Versioned

object Order {

	import Event._
	import AggregateRoot._
	
	def rehydrate(history: Event*): Order = rehydrate(history.toList)
	def rehydrate(history: List[Event]): Order = evolve(empty)(history)

	//	Commands
	lazy val createFor: UUID => String => StateTransition[Order] =
		id => descr => newStateTransition(_ => OrderCreated(id, descr, 1) :: Nil)

	lazy val addInventoryItemToOrder: UUID => Int => StateTransition[Order] =
		inventoryItemId => quantity => 
			newStateTransition(
				ord => if(canBeChanged(ord)) InventoryItemAddedToOrder(ord.id, inventoryItemId, quantity, ord.expectedNextVersion) :: Nil
					   else Nil // TODO: Error, cannot be changed
			)

	lazy val removeInventoryItemFromOrder: UUID => Int => StateTransition[Order] = 
		inventoryItemId => quantity => 
			newStateTransition(
				ord =>	if(canBeChanged(ord))
							if(canRemoveTheItem(ord)(inventoryItemId)(quantity))
								InventoryItemRemovedFromOrder(ord.id, inventoryItemId, quantity, ord.expectedNextVersion) :: Nil
							else
								Nil // TODO: Error, not enough items to remove
						else
							Nil // TODO: Error, cannot be changed
			)

	lazy val addShippingAddressToOrder: String => StateTransition[Order] =
		shippingAddress => newStateTransition(
			ord =>	if(canBeChanged(ord)) ShippingAddressAddedToOrder(ord.id, shippingAddress, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, cannot be changed
		)

	lazy val payTheBalance: StateTransition[Order] = 
		newStateTransition(
			ord => 	if(canBePayed(ord)) OrderPayed(ord.id, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, cannot be payed twice
		)

	lazy val submit: StateTransition[Order] = 
		newStateTransition(
			ord => 	if(canBeSubmitted(ord)) OrderSubmitted(ord.id, ord.expectedNextVersion) :: Nil
					else Nil // TODO: Error, the order cannot be submitted
		)

	//	Aggregate Evolution
	lazy val newState: Order => Event => Order = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case OrderCreated(newId, description, sequence) =>
					Order(newId, description, "", isPayed = false, OrderItems.empty, Open, sequence)

				case InventoryItemAddedToOrder(_, inventoryItemId, quantity, sequence) => 
					addItems(inventoryItemId)(quantity)(sequence)(aggregate)
					
				case InventoryItemRemovedFromOrder(_, inventoryItemId, quantity, sequence) => 
					removeItems(inventoryItemId)(quantity)(sequence)(aggregate)
					
				case ShippingAddressAddedToOrder(_, shippingAddress, sequence) => 
					applyAddress(shippingAddress)(sequence)(aggregate)

				case OrderPayed(_, sequence) =>
					afterPayed(sequence)(aggregate)

				case OrderSubmitted(_, sequence) =>
					afterSubmitted(sequence)(aggregate)
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	private lazy val empty = Order(
		id = new UUID(0, 0),
		description = "",
		shippingAddress = "",
		isPayed = false,
		items = OrderItems.empty,
		status = Open,
		version = 0
	)

	//	Validation
	private lazy val canRemoveTheItem: Order => UUID => Int => Boolean = 
		ord => itemId => quantity => (ord.items get itemId).fold(false){ _ >= quantity }

	private lazy val canBeChanged: Order => Boolean = 
		ord => ord.status == Open

	private lazy val theShippingAddressIsValid: Order => Boolean = 
		ord => !ord.shippingAddress.isEmpty

	private lazy val canBePayed: Order => Boolean = 
		ord => !ord.isPayed

	private lazy val canBeSubmitted: Order => Boolean = 
		ord => ord.isPayed && theShippingAddressIsValid(ord)

	private lazy val canBeDispatched: Order => Boolean = 
		ord => (
			ord.status == AllItemsAvailable 
			&& ord.isPayed 
			&& theShippingAddressIsValid(ord)
		)

	private lazy val canBeVoided: Order => Boolean = 
		ord => ord.status != Dispatched

	//	Lenses
	private lazy val applyAddress: String => Long => Order => Order =
		addr => ver => Order.version.set(ver) compose Order.shippingAddress.set(addr)

	private lazy val afterPayed: Long => Order => Order =
		ver => Order.version.set(ver) compose Order.isPayed.set(true)

	private lazy val afterSubmitted: Long => Order => Order =
		ver => Order.version.set(ver) compose Order.status.set(Submitted)

	private lazy val addItems: UUID => Int => Long => Order => Order =
		itemId => count => ver => Order.version.set(ver) compose atItemEvaluate(itemId)(_ + count)

	private lazy val removeItems: UUID => Int => Long => Order => Order =
		itemId => count => ver => Order.version.set(ver) compose atItemEvaluate(itemId)(_ - count)

	private lazy val atItemEvaluate: UUID => (Int => Int) => Order => Order =
		itemId => f => atItem(itemId) modify (prev => f(prev getOrElse 0).some)

	private lazy val atItem: UUID => Lens[Order, Option[Int]] =
		itemId => Order.items composeLens at(itemId)
}
