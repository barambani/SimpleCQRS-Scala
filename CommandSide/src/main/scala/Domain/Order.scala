package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import monocle.macros.Lenses
import monocle.{Lens, Optional}
import monocle.macros.GenLens
import monocle.function.all.at
import monocle.std.map._
import scalaz.Scalaz._
import Events._
import Events.Event._
import DomainAggregates._
import AggregateRoot._

import OrderItems._

object OrderItems {	
	type OrderItems = Map[UUID, Int]
	lazy val empty: OrderItems = Map.empty
}

sealed trait OrderStatus extends Product with Serializable
sealed trait Open extends OrderStatus
sealed trait Submitted extends OrderStatus
final case object Open extends Open
final case object Submitted extends Submitted

@Lenses final case class Order private (
	id: UUID,
	description: String,
	shippingAddress: String,
	isPayed: Boolean,
	items: OrderItems,
	status: OrderStatus,
	version: Long) extends Identity with Versioned

object Order {

	lazy val empty = Order(
		id = Event.zeroEvent.id,
		description = "",
		shippingAddress = "",
		isPayed = false,
		items = OrderItems.empty,
		status = Open,
		version = Event.zeroEvent.sequence
	)
	
	def rehydrate(history: Event*): Order = rehydrate(history.toList)
	def rehydrate(history: List[Event]): Order = evolve(empty)(history)

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
