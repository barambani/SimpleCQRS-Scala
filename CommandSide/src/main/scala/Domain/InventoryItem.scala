package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds

import scalaz._

@Lenses case class InventoryItem private[InventoryItem] (
	id: UUID = new UUID(0, 0), 
	name: String = "", 
	isActive: Boolean = false,
	itemsCount: Int = 0,
	version: Long = 0) extends Identity with Versioned {
	
	//	Domain logic
	lazy val countAfterCheckIn: Int => Int = toCheckin => itemsCount + toCheckin
	lazy val countAfterRemoval: Int => Int = toRemove => itemsCount - toRemove
	lazy val itemsAvailableInStock: Int => Boolean = count => itemsCount >= count
	lazy val itemsMissingToFulfil: Int => Int = count => count - itemsCount
}

object InventoryItem {

	import Event._
	import AggregateRoot._

	//	Factories
	def apply(history: Event*): InventoryItem = apply(history.toList)
	def apply(history: List[Event]): InventoryItem = evolve(new InventoryItem, history)

	//	Commands
	lazy val deactivateInventoryItem: () => StateTransition[InventoryItem] = 
		() => newStateTransition(item => InventoryItemDeactivated(item.id, item.expectedNextVersion) :: Nil)

	lazy val checkInItemsToInventory: Int => StateTransition[InventoryItem] =
		count => newStateTransition(item => ItemsCheckedInToInventory(item.id, count, item.expectedNextVersion) :: Nil)

	lazy val renameInventoryItem: String => StateTransition[InventoryItem] = 
		newName => newStateTransition(
			item =>
				if(theNameIsValid(newName)) InventoryItemRenamed(item.id, newName, item.expectedNextVersion) :: Nil 
				else Nil // TODO: Error, the new name is not valid
		)
	
	lazy val removeItemsFromInventory: Int => StateTransition[InventoryItem] = 
		count => newStateTransition(
			item =>
				if(item.itemsAvailableInStock(count)) ItemsRemovedFromInventory(item.id, count, item.expectedNextVersion) :: Nil
				else Nil // TODO: Error, no items left
		)

	//	Aggregate Evolution
	lazy val newState: InventoryItem => Event => InventoryItem = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					new InventoryItem(newId, newName, true, version = sequence)
				
				case InventoryItemDeactivated(_, sequence) =>
					withActiveStatus(false)(sequence)(aggregate)

				case InventoryItemRenamed(_, newName, sequence) => 
					withName(newName)(sequence)(aggregate)

				case ItemsCheckedInToInventory(_, count, sequence) => 
					withCheckedInItems(aggregate.countAfterCheckIn(count))(sequence)(aggregate)
				
				case ItemsRemovedFromInventory(_, count, sequence) => 
					withCheckedInItems(aggregate.countAfterRemoval(count))(sequence)(aggregate)
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	//	Lenses
	private lazy val withName: String => Long => InventoryItem => InventoryItem =
		n => ver => InventoryItem.version.set(ver) compose InventoryItem.name.set(n)

	private lazy val withCheckedInItems: Int => Long => InventoryItem => InventoryItem =
		is => ver => InventoryItem.version.set(ver) compose InventoryItem.itemsCount.set(is)

	private lazy val withActiveStatus: Boolean => Long => InventoryItem => InventoryItem =
		a => ver => InventoryItem.version.set(ver) compose InventoryItem.isActive.set(a)

	//	Validation
	private lazy val theNameIsValid: String => Boolean = n => !n.isEmpty
}
