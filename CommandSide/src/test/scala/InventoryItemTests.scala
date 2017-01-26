package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain._

import scalaz._

import java.util.UUID

object InventoryItemSpec extends Specification {

	import InventoryItem._
	import AggregateRoot._
	import DomainState._

	val id = UUID.randomUUID

	"InventoryItem" should {

	  	"have the correct state after the InventoryItemCreated event" in {

	  		val event = InventoryItemCreated(id, "Test Inventory Item", 1)

	  		val finalState = InventoryItem.rehydrate(event)

  			finalState.id mustEqual id
			finalState.name mustEqual "Test Inventory Item"
			finalState.isActive mustEqual true
	  	}

	  	"change state correctly after receiving in sequence events" in {

	  		val history = List(
	  			InventoryItemDeactivated(id, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)

  			val finalState = InventoryItem.rehydrate(history)

  			finalState.version mustEqual 2
  			finalState.isActive mustEqual false
	  	}

	  	"not change state after receiving out of sequence events" in {

	  		val history = List(
	  			InventoryItemDeactivated(id, 3),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)

  			val finalState = InventoryItem.rehydrate(history)

  			finalState.version mustEqual 1
  			finalState.isActive mustEqual true
	  	}

	  	"have the correct name after the InventoryItemRenamed event" in {

  			val history = List(
	  			InventoryItemRenamed(id, "New Iten Name", 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		val finalState = InventoryItem.rehydrate(history)

			finalState.name mustEqual "New Iten Name"
	  	}

	  	"have the correct item count after the check in and removal of items" in {

  			val history = List(
  				ItemsRemovedFromInventory(id, 3, 5),
  				ItemsCheckedInToInventory(id, 2, 4),
  				ItemsRemovedFromInventory(id, 3, 3),
  				ItemsCheckedInToInventory(id, 10, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		val finalState = InventoryItem.rehydrate(history)

			finalState.itemsCount mustEqual 6
	  	}

	  	"generate the correct events after one command application" in {

  			val history = List(
  				ItemsCheckedInToInventory(id, 10, 3),
  				ItemsCheckedInToInventory(id, 10, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		lazy val item = InventoryItem.rehydrate(history)

	  		execTransition(removeItemsFromInventory(2))(item).itemsCount mustEqual 18
	  		execTransition(removeItemsFromInventory(2))(item).version mustEqual 4
	  	}

	  	"have the correct state after one command application" in {

  			val history = List(
  				ItemsCheckedInToInventory(id, 10, 3),
  				ItemsCheckedInToInventory(id, 10, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		lazy val item = InventoryItem.rehydrate(history)

	  		evalTransition(removeItemsFromInventory(7))(item) match {
	  			case ItemsRemovedFromInventory(i, c, s) :: Nil => {
					i mustEqual id
					c mustEqual 7
					s mustEqual 4
				}
				case _ => ko("The generated event is not of the correct type")
	  		}
	  	}

	  	"generate the correct events after commands application" in {

  			val history = List(
  				ItemsCheckedInToInventory(id, 10, 3),
  				ItemsCheckedInToInventory(id, 10, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		lazy val item = InventoryItem.rehydrate(history)
	  		lazy val transitions = Seq(removeItemsFromInventory(2), removeItemsFromInventory(7))

			evalTransitions(transitions)(item).head.sequence mustEqual 5
	  	}

	  	"have the correct state after commands application" in {

  			val history = List(
  				ItemsCheckedInToInventory(id, 10, 3),
  				ItemsCheckedInToInventory(id, 10, 2),
	  			InventoryItemCreated(id, "Test Inventory Item", 1)
  			)
	  		
	  		lazy val item = InventoryItem.rehydrate(history)
	  		lazy val transitions = Seq(removeItemsFromInventory(2), removeItemsFromInventory(7), checkInItemsToInventory(3))
			
			lazy val newTransition: StateTransition[InventoryItem] = mergeTransitions(transitions)

	  		execTransition(newTransition)(item).itemsCount mustEqual 14
	  		execTransition(newTransition)(item).version mustEqual 6
	  	}
	}
}