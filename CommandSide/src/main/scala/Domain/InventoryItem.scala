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

	import EventOps._
	import AggregateRoot._

	def apply(history: Event*): InventoryItem = apply(history.toList)
	def apply(history: List[Event]): InventoryItem = evolve(new InventoryItem, history)

	//	Commands
	def deactivateInventoryItem: StateTransition[InventoryItem] = 
		newStateTransition(item => InventoryItemDeactivated(item.id, nextStateVersion(item)) :: Nil)

	def checkInItemsToInventory(count: Int): StateTransition[InventoryItem] =
		newStateTransition(item => ItemsCheckedInToInventory(item.id, count, nextStateVersion(item)) :: Nil)

	def renameInventoryItem(newName: String): StateTransition[InventoryItem] = 
		newStateTransition(
			item => if(theNameIsValid(newName)) InventoryItemRenamed(item.id, newName, nextStateVersion(item)) :: Nil 
				 else Nil // TODO: Error, the new name is not valid
		)
	
	def removeItemsFromInventory(count: Int): StateTransition[InventoryItem] = 
		newStateTransition(
			item => if(item.itemsCanBeRemoved(count)) ItemsRemovedFromInventory(item.id, count, nextStateVersion(item)) :: Nil
				 else Nil // TODO: Error, not enough items to remove
		)

	//	Evolution
	lazy val newState: InventoryItem => Event => InventoryItem = 
		aggregate => event =>
			if(!hasACorrectIdCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequenceCheck(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					if(theNameIsValid(newName)) new InventoryItem(newId, newName, true, version = sequence)
					else aggregate // TODO: Error, the new name is not valid
				
				case InventoryItemDeactivated(_, sequence) =>
					withActiveStatus(false, sequence)(aggregate)

				case InventoryItemRenamed(_, newName, sequence) => 
					if(theNameIsValid(newName)) withName(newName, sequence)(aggregate)
					else aggregate // TODO: Error, the new name is not valid

				case ItemsCheckedInToInventory(_, count, sequence) => 
					withCheckedInItems(aggregate.countAfterCheckIn(count), sequence)(aggregate)
				
				case ItemsRemovedFromInventory(_, count, sequence) => 
					if(aggregate.itemsCanBeRemoved(count)) withCheckedInItems(aggregate.countAfterRemoval(count), sequence)(aggregate)
					else aggregate // TODO: Error, not enough items to remove
				
				case _ => aggregate // TODO: log event ignored with event details
			}

	private lazy val withName: (String, Long) => InventoryItem => InventoryItem =
		(n, s) => InventoryItem.version.set(s) compose InventoryItem.name.set(n)

	private lazy val withCheckedInItems: (Int, Long) => InventoryItem => InventoryItem =
		(is, s) => InventoryItem.version.set(s) compose InventoryItem.itemsCount.set(is)

	private lazy val withActiveStatus: (Boolean, Long) => InventoryItem => InventoryItem =
		(a, s) => InventoryItem.version.set(s) compose InventoryItem.isActive.set(a)

	private lazy val theNameIsValid: String => Boolean = n => !n.isEmpty
}
