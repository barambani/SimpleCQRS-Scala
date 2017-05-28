package SimpleCqrsScala.CommandSide.Services

import SimpleCqrsScala.CommandSide.Domain.InventoryItem
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import SimpleCqrsScala.CommandSide.Domain.DomainState.EitherTransition._
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._
import SimpleCqrsScala.CommandSide.Validation.InventoryItemValidation

import java.util.UUID

trait InventoryItemService extends InventoryItemValidation {

	def createItemFor(id: UUID, name: String): EitherTransition[InventoryItem] =
		liftValidated {
			theNameIsValid(id, None)(name) map { 
				_ => InventoryItemCreated(id, name, 1) :: Nil
			}
		}
	
	def renameInventoryItem(newName: String): EitherTransition[InventoryItem] = 
		liftValidatedF {
			item => theNameIsValid(item.id, Some(item.name))(newName) map { 
				_ => InventoryItemRenamed(item.id, newName, item.expectedNextVersion) :: Nil
			}
		}
	
	def removeItemsFromInventory(count: Int): EitherTransition[InventoryItem] =
		liftValidatedF {
			item => availableInStock(item)(count) map { 
				_ => ItemsRemovedFromInventory(item.id, count, item.expectedNextVersion) :: Nil
			}
		}

	def checkInItemsToInventory(count: Int): EitherTransition[InventoryItem] =
		liftEventsF {
			item => ItemsCheckedInToInventory(item.id, count, item.expectedNextVersion) :: Nil
		}

	def deactivateInventoryItem: EitherTransition[InventoryItem] = 
		liftEventsF {
			item => InventoryItemDeactivated(item.id, item.expectedNextVersion) :: Nil
		}
}