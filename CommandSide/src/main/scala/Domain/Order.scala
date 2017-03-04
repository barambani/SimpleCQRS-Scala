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
import scalaz.{\/, -\/, \/-}
import Validator._

import OrderItems._

object OrderItems {	
	type OrderItems = Map[UUID, Int]
	lazy val empty: OrderItems = Map.empty
}

sealed trait OrderStatus extends Product with Serializable
final case object Open extends OrderStatus
final case object Submitted extends OrderStatus

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
	import DomainAggregates._
	import AggregateRoot._
	import EitherTransition._
	
	def rehydrate(history: Event*): Order = rehydrate(history.toList)
	def rehydrate(history: List[Event]): Order = evolve(empty)(history)

	//	Commands
	def createFor(id: UUID, customerId: UUID, customerName: String): EitherTransition[Order] =
		newTransitionA(
			OrderCreated(id, s"Order for $customerName (id: $customerId)", 1) :: Nil
		)

	def addInventoryItemToOrder(itemId: UUID, quantity: Int): EitherTransition[Order] =
		newTransitionV(
			ord => validation.apply(canBeChanged(ord)) { 
				_ => InventoryItemAddedToOrder(ord.id, itemId, quantity, ord.expectedNextVersion) :: Nil
			}
		)

	def removeInventoryItemFromOrder(itemId: UUID, quantity: Int): EitherTransition[Order] = 
		newTransitionV(
			ord => validation.apply2(canBeChanged(ord), hasEnoughItems(ord)(itemId, quantity)) { 
				(_, _) => InventoryItemRemovedFromOrder(ord.id, itemId, quantity, ord.expectedNextVersion) :: Nil
			}
		)

	def addShippingAddressToOrder(address: String): EitherTransition[Order] =
		newTransitionV(
			ord => validation.apply2(canBeChanged(ord), shippingAddressValid(ord)(address)) { 
				(_, _) => ShippingAddressAddedToOrder(ord.id, address, ord.expectedNextVersion) :: Nil
			}
		)

	def payTheBalance: EitherTransition[Order] =
		newTransitionV(
			ord => validation.apply2(canBeChanged(ord), canBePayed(ord)) { 
				(_, _) => OrderPayed(ord.id, ord.expectedNextVersion) :: Nil
			}
		)

	def submit: EitherTransition[Order] = 
		newTransitionV(
			ord => validation.apply3(canBeChanged(ord), containsItems(ord), isPaymentValid(ord)) { 
				(_, _, _) => OrderSubmitted(ord.id, ord.expectedNextVersion) :: Nil
			}
		)


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

	lazy val empty = Order(
		id = Event.zeroEvent.id,
		description = "",
		shippingAddress = "",
		isPayed = false,
		items = OrderItems.empty,
		status = Open,
		version = Event.zeroEvent.sequence
	)

	//	Validation
	private def canBeChanged(ord: Order): Validated[Order] = 
		ord.status match {
			case Open	=> \/-(ord)
			case _	 	=> -\/(OrderClosed(ord.id, ord.description))
		}

	private def shippingAddressValid(ord: Order)(shippingAddress: String): Validated[String] = 
		shippingAddress.isEmpty match {
			case true 	=> -\/(ShippingAddressNotValid(ord.id, ord.description, shippingAddress))
			case false 	=> \/-(shippingAddress)
		}

	private def hasEnoughItems(ord: Order)(itemId: UUID, quantity: Int): Validated[(UUID, Int)] = 
		(ord.items get itemId).fold(false){ _ >= quantity } match {
			case true 	=> \/-((itemId, quantity))
			case false 	=> -\/(NotEnoughItemsInTheOrder(ord.id, ord.description, itemId, quantity))
		}

	private def isPaymentValid(ord: Order): Validated[Order] = 
		ord.isPayed match {
			case true 	=> \/-(ord)
			case false 	=> -\/(OrderPaymentIsNotValid(ord.id, ord.description))
		}

	private def canBePayed(ord: Order): Validated[Order] = 
		ord.isPayed match {
			case false 	=> \/-(ord)
			case true 	=> -\/(OrderAlreadyPayed(ord.id, ord.description))
		}

	private def containsItems(ord: Order): Validated[Order] =
		ord.items.isEmpty match {
			case false 	=> \/-(ord)
			case true 	=> -\/(OrderContainsNoItems(ord.id, ord.description))
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
