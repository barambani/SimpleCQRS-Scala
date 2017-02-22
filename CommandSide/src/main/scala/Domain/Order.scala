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
import scalaz.Reader

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
	lazy val createFor: UUID => String => EitherTransition[Order] =
		id => descr => newTransition(Reader(_ => OrderCreated(id, descr, 1) :: Nil))

	lazy val addInventoryItemToOrder: UUID => Int => Order => EitherTransition[Order] =
		itemId => quantity => ord =>
			if(!canBeChanged(ord)) failedTransition(OrderClosed(ord.id, ord.description))
			else 				   newTransition(Reader(o => InventoryItemAddedToOrder(o.id, itemId, quantity, o.expectedNextVersion) :: Nil))

	lazy val removeInventoryItemFromOrder: UUID => Int => Order => EitherTransition[Order] = 
		itemId => quantity => ord => 
			if(!canBeChanged(ord)) 					   failedTransition(OrderClosed(ord.id, ord.description))
			else if(!hasEnough(itemId)(quantity)(ord)) failedTransition(NotEnoughItemsInTheOrder(ord.id, ord.description, itemId, quantity))
			else 												
				newTransition(Reader(o => InventoryItemRemovedFromOrder(o.id, itemId, quantity, o.expectedNextVersion) :: Nil))

	lazy val addShippingAddressToOrder: String => Order => EitherTransition[Order] =
		address => ord => 
			if(!canBeChanged(ord)) 					failedTransition(OrderClosed(ord.id, ord.description))
			else if(!shippingAddressValid(address))	failedTransition(ShippingAddressNotValid(ord.id, ord.description, address))
			else 									newTransition(Reader(o => ShippingAddressAddedToOrder(ord.id, address, ord.expectedNextVersion) :: Nil))

	lazy val payTheBalance: Order => EitherTransition[Order] =
		ord =>
			if(!canBeChanged(ord)) 		failedTransition(OrderClosed(ord.id, ord.description))
			else if(!canBePayed(ord))	failedTransition(OrderAlreadyPayed(ord.id, ord.description))
			else 						newTransition(Reader(o => OrderPayed(o.id, o.expectedNextVersion) :: Nil))

	lazy val submit: Order => EitherTransition[Order] = 
		ord =>
			if(!canBeChanged(ord)) 			failedTransition(OrderClosed(ord.id, ord.description))
			else if(!canBeSubmitted(ord))	failedTransition(OrderNotComplete(ord.id, ord.description))
			else 							newTransition(Reader(o => OrderSubmitted(o.id, o.expectedNextVersion) :: Nil))

	//	Aggregate Evolution
	lazy val newState: Order => Event => Order = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case OrderCreated(newId, description, sequence) =>
					Order(newId, description, "", isPayed = false, OrderItems.empty, Open, sequence)

				case InventoryItemAddedToOrder(_, itemId, quantity, sequence) => 
					addItems(itemId)(quantity)(sequence)(aggregate)
					
				case InventoryItemRemovedFromOrder(_, itemId, quantity, sequence) => 
					removeItems(itemId)(quantity)(sequence)(aggregate)
					
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

	//	Rules
	private lazy val hasEnough: UUID => Int => Order => Boolean = 
		itemId => quantity => ord => (ord.items get itemId).fold(false){ _ >= quantity }

	private lazy val canBeChanged: Order => Boolean = 
		ord => ord.status == Open

	private lazy val shippingAddressValid: String => Boolean = 
		shippingAddress => !shippingAddress.isEmpty

	private lazy val canBePayed: Order => Boolean = 
		ord => !ord.isPayed

	private lazy val canBeSubmitted: Order => Boolean = 
		ord => ord.isPayed && shippingAddressValid(ord.shippingAddress)

	private lazy val canBeDispatched: Order => Boolean = 
		ord => (
			ord.status == AllItemsAvailable 
			&& ord.isPayed 
			&& shippingAddressValid(ord.shippingAddress)
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
