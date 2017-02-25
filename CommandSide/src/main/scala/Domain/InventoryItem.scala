package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds
import scalaz.{\/,-\/,\/-}
import scalaz.Reader

@Lenses final case class InventoryItem private (
	id: UUID, 
	name: String, 
	isActive: Boolean, 
	itemsCount: Int, 
	version: Long) extends Identity with Versioned

object InventoryItem {

	import Event._
	import AggregateRoot._

	def rehydrate(history: Event*): InventoryItem = rehydrate(history.toList)
	def rehydrate(history: List[Event]): InventoryItem = evolve(empty)(history)

	//	Commands
	lazy val createFor: UUID => String => EitherTransition[InventoryItem] =
		id => name => 
			if(!theNameIsValid(name)) failedTransition(InventoryItemNameNotValid(id, None, name))
			else 					  newTransition(Reader(_ => InventoryItemCreated(id, name, 1) :: Nil))

	lazy val deactivateInventoryItem: EitherTransition[InventoryItem] = 
		newTransition(Reader(i => InventoryItemDeactivated(i.id, i.expectedNextVersion) :: Nil))

	lazy val renameInventoryItem: String => InventoryItem => EitherTransition[InventoryItem] = 
		newName => item => 
			if(!theNameIsValid(newName)) failedTransition(InventoryItemNameNotValid(item.id, Some(item.name), newName))
			else 						 newTransition(Reader(i => InventoryItemRenamed(i.id, newName, i.expectedNextVersion) :: Nil))

	lazy val checkInItemsToInventory: Int => EitherTransition[InventoryItem] =
		count => newTransition(Reader(i => ItemsCheckedInToInventory(i.id, count, i.expectedNextVersion) :: Nil))
	
	lazy val removeItemsFromInventory: Int => InventoryItem => EitherTransition[InventoryItem] = 
		count => item =>
			if(!availableInStock(item)(count)) failedTransition(NotEnoughItemsInStock(item.id, item.name, count))
			else 							   newTransition(Reader(i => ItemsRemovedFromInventory(i.id, count, i.expectedNextVersion) :: Nil))

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

	private lazy val empty = InventoryItem(id = new UUID(0, 0), name = "", isActive = false, itemsCount = 0, version = 0)

	//	Validation
	private lazy val theNameIsValid: String => Boolean = 
		n => !n.isEmpty

	private lazy val availableInStock: InventoryItem => Int => Boolean = 
		i => count => i.itemsCount >= count

	//	Lenses
	private lazy val applyName: String => Long => InventoryItem => InventoryItem =
		n => ver => InventoryItem.version.set(ver) compose InventoryItem.name.set(n)

	private lazy val applyCheckedInCount: Int => Long => InventoryItem => InventoryItem =
		is => ver => InventoryItem.version.set(ver) compose InventoryItem.itemsCount.set(is)

	private lazy val applyStatus: Boolean => Long => InventoryItem => InventoryItem =
		a => ver => InventoryItem.version.set(ver) compose InventoryItem.isActive.set(a)
}
