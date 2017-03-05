package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import monocle.macros.Lenses
import Events._
import Events.Event._
import DomainAggregates._
import AggregateRoot._

import SimpleCqrsScala.CommandSide.Domain.Errors._

@Lenses final case class InventoryItem private (
	id: UUID, 
	name: String, 
	isActive: Boolean, 
	itemsCount: Int, 
	version: Long) extends Identity with Versioned

object InventoryItem {

	lazy val empty = InventoryItem(
		id = Event.zeroEvent.id, name = "", 
		isActive = false, 
		itemsCount = 0, 
		version = Event.zeroEvent.sequence
	)

	def rehydrate(history: Event*): InventoryItem = rehydrate(history.toList)
	def rehydrate(history: List[Event]): InventoryItem = evolve(empty)(history)
		
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

	//	Lenses
	private lazy val applyName: String => Long => InventoryItem => InventoryItem =
		n => ver => InventoryItem.version.set(ver) compose InventoryItem.name.set(n)

	private lazy val applyCheckedInCount: Int => Long => InventoryItem => InventoryItem =
		is => ver => InventoryItem.version.set(ver) compose InventoryItem.itemsCount.set(is)

	private lazy val applyStatus: Boolean => Long => InventoryItem => InventoryItem =
		a => ver => InventoryItem.version.set(ver) compose InventoryItem.isActive.set(a)
}
