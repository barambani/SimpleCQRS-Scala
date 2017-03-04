package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds
import scalaz.{\/,-\/,\/-}
import scalaz.Reader
import Validator._

@Lenses final case class InventoryItem private (
	id: UUID, 
	name: String, 
	isActive: Boolean, 
	itemsCount: Int, 
	version: Long) extends Identity with Versioned

object InventoryItem {

	import Event._
	import DomainAggregates._
	import AggregateRoot._
	import EitherTransition._

	def rehydrate(history: Event*): InventoryItem = rehydrate(history.toList)
	def rehydrate(history: List[Event]): InventoryItem = evolve(empty)(history)

	//	Commands
	def createFor(id: UUID, name: String): EitherTransition[InventoryItem] =
		liftValidated(
			validation.apply(theNameIsValid(id, None)(name)) { 
				_ => InventoryItemCreated(id, name, 1) :: Nil
			}
		)
	
	def renameInventoryItem(newName: String): EitherTransition[InventoryItem] = 
		liftValidatedF(
			item => validation.apply(theNameIsValid(item.id, Some(item.name))(newName)) { 
				_ => InventoryItemRenamed(item.id, newName, item.expectedNextVersion) :: Nil
			}
		)
	
	def removeItemsFromInventory(count: Int): EitherTransition[InventoryItem] =
		liftValidatedF(
			item => validation.apply(availableInStock(item)(count)) { 
				_ => ItemsRemovedFromInventory(item.id, count, item.expectedNextVersion) :: Nil
			}
		) 

	def checkInItemsToInventory(count: Int): EitherTransition[InventoryItem] =
		liftEventsF(item => ItemsCheckedInToInventory(item.id, count, item.expectedNextVersion) :: Nil)

	def deactivateInventoryItem: EitherTransition[InventoryItem] = 
		liftEventsF(item => InventoryItemDeactivated(item.id, item.expectedNextVersion) :: Nil)

		
	//	Aggregate Evolution
	lazy val newState: InventoryItem => Event => InventoryItem = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					InventoryItem(newId, newName, true, 0, sequence)
				
				case InventoryItemDeactivated(_, sequence) =>
					applyStatus(false)(sequence)(aggregate)

				case InventoryItemRenamed(_, newName, sequence) => 
					applyName(newName)(sequence)(aggregate)

				case ItemsCheckedInToInventory(_, count, sequence) => 
					applyCheckedInCount(aggregate.itemsCount + count)(sequence)(aggregate)
				
				case ItemsRemovedFromInventory(_, count, sequence) => 
					applyCheckedInCount(aggregate.itemsCount - count)(sequence)(aggregate)
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	lazy val empty = InventoryItem(
		id = Event.zeroEvent.id, name = "", 
		isActive = false, 
		itemsCount = 0, 
		version = Event.zeroEvent.sequence
	)

	//	Validation
	private def theNameIsValid(id: UUID, actualName: Option[String])(name: String): Validated[String] = 
		name.isEmpty match {
			case true 	=> -\/(InventoryItemNameNotValid(id, actualName, name))
			case false 	=> \/-(name)
		}

	private def availableInStock(item: InventoryItem)(count: Int): Validated[Int] = 
		item.itemsCount >= count match {
			case true 	=> \/-(count)
			case false 	=> -\/(NotEnoughItemsInStock(item.id, item.name, count))
		}

	//	Lenses
	private lazy val applyName: String => Long => InventoryItem => InventoryItem =
		n => ver => InventoryItem.version.set(ver) compose InventoryItem.name.set(n)

	private lazy val applyCheckedInCount: Int => Long => InventoryItem => InventoryItem =
		is => ver => InventoryItem.version.set(ver) compose InventoryItem.itemsCount.set(is)

	private lazy val applyStatus: Boolean => Long => InventoryItem => InventoryItem =
		a => ver => InventoryItem.version.set(ver) compose InventoryItem.isActive.set(a)
}
