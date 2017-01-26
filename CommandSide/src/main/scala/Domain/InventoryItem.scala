package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import monocle.macros.Lenses
import scala.language.higherKinds
import scalaz.{\/, -\/, \/-}

@Lenses case class InventoryItem private (id: UUID, name: String, isActive: Boolean, itemsCount: Int, version: Long) extends Identity with Versioned

object InventoryItem {

	import Event._
	import AggregateRoot._

	private lazy val empty = InventoryItem(id = new UUID(0, 0), name = "", isActive = false, itemsCount = 0, version = 0)

	def rehydrate(history: Event*): InventoryItem = rehydrate(history.toList)
	def rehydrate(history: List[Event]): InventoryItem = evolve(empty)(history)

	//	Validation
	private lazy val theNameIsValid: String => Boolean = 
		n => !n.isEmpty

	private lazy val itemsAreAvailableInStock: InventoryItem => Int => Boolean = 
		i => count => i.itemsCount >= count

	//	Commands
	// lazy val createFor: UUID => String => Throwable \/ StateTransition[InventoryItem] =
	// 	id => name => 
	// 		if(theNameIsValid(name)) 
	// 			\/-(
	// 				newStateTransition(_ => InventoryItemCreated(id, name, 1) :: Nil)
	// 			)
	// 		else 
	// 			-\/(new Exception)

	lazy val createFor: UUID => String => StateTransition[InventoryItem] =
		id => name => 
			newStateTransition(
				item => if(theNameIsValid(name)) InventoryItemCreated(id, name, 1) :: Nil
						else Nil //	Error, cannot create the item 
			)

	lazy val deactivateInventoryItem: () => StateTransition[InventoryItem] = 
		() => newStateTransition(item => InventoryItemDeactivated(item.id, item.expectedNextVersion) :: Nil)

	lazy val renameInventoryItem: String => StateTransition[InventoryItem] = 
		newName => newStateTransition(
			item =>
				if(theNameIsValid(newName)) InventoryItemRenamed(item.id, newName, item.expectedNextVersion) :: Nil 
				else Nil // TODO: Error, the new name is not valid
		)

	lazy val checkInItemsToInventory: Int => StateTransition[InventoryItem] =
		count => newStateTransition(item => ItemsCheckedInToInventory(item.id, count, item.expectedNextVersion) :: Nil)
	
	lazy val removeItemsFromInventory: Int => StateTransition[InventoryItem] = 
		count => newStateTransition(
			item =>
				if(itemsAreAvailableInStock(item)(count)) ItemsRemovedFromInventory(item.id, count, item.expectedNextVersion) :: Nil
				else Nil // TODO: Error, no items left
		)

	//	Aggregate Evolution
	lazy val newState: InventoryItem => Event => InventoryItem = 
		aggregate => event =>
			if(!hasACorrectId(event)(aggregate)) aggregate // TODO: Error in this case
			else if(!isInSequence(event)(aggregate)) aggregate // TODO: Error in this case
			else event match {
				case InventoryItemCreated(newId, newName, sequence) => 
					InventoryItem(newId, newName, true, 0, version = sequence)
				
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

	//	Lenses
	private lazy val applyName: String => Long => InventoryItem => InventoryItem =
		n => ver => InventoryItem.version.set(ver) compose InventoryItem.name.set(n)

	private lazy val applyCheckedInCount: Int => Long => InventoryItem => InventoryItem =
		is => ver => InventoryItem.version.set(ver) compose InventoryItem.itemsCount.set(is)

	private lazy val applyStatus: Boolean => Long => InventoryItem => InventoryItem =
		a => ver => InventoryItem.version.set(ver) compose InventoryItem.isActive.set(a)
}
