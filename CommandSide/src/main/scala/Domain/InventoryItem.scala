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
	lazy val itemsCanBeRemoved: Int => Boolean = count => itemsCount >= count
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
				if(item.itemsCanBeRemoved(count)) ItemsRemovedFromInventory(item.id, count, item.expectedNextVersion) :: Nil
				else Nil // TODO: Error, not enough items to remove
		)

	//	Evolution
	lazy val newState: InventoryItem => Event => InventoryItem = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					if(theNameIsValid(newName)) new InventoryItem(newId, newName, true, version = sequence)
					else aggregate // TODO: Error, the new name is not valid
				
				case InventoryItemDeactivated(_, sequence) =>
					getNewWithActiveStatus(false)(sequence)(aggregate)

				case InventoryItemRenamed(_, newName, sequence) => 
					if(theNameIsValid(newName)) getNewWithName(newName)(sequence)(aggregate)
					else aggregate // TODO: Error, the new name is not valid

				case ItemsCheckedInToInventory(_, count, sequence) => 
					getNewWithCheckedInItems(aggregate.countAfterCheckIn(count))(sequence)(aggregate)
				
				case ItemsRemovedFromInventory(_, count, sequence) => 
					if(aggregate.itemsCanBeRemoved(count)) getNewWithCheckedInItems(aggregate.countAfterRemoval(count))(sequence)(aggregate)
					else aggregate // TODO: Error, not enough items to remove
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	private lazy val getNewWithName: String => Long => InventoryItem => InventoryItem =
		n => ver => InventoryItem.version.set(ver) compose InventoryItem.name.set(n)

	private lazy val getNewWithCheckedInItems: Int => Long => InventoryItem => InventoryItem =
		is => ver => InventoryItem.version.set(ver) compose InventoryItem.itemsCount.set(is)

	private lazy val getNewWithActiveStatus: Boolean => Long => InventoryItem => InventoryItem =
		a => ver => InventoryItem.version.set(ver) compose InventoryItem.isActive.set(a)

	private lazy val theNameIsValid: String => Boolean = n => !n.isEmpty
}
