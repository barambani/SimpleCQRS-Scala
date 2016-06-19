package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain._

import scalaz._
import Scalaz._

import java.util.UUID

object OrderSpec extends Specification {

	import AggregateRoot._
	import DomainStates._

	val orderId = UUID.randomUUID
	val inventoryItemId1 = UUID.randomUUID
	val inventoryItemId2 = UUID.randomUUID
	val inventoryItemId3 = UUID.randomUUID

	"An order" should {

		"have the correct number of items after a serie of additions" in {

	  		val history = List(
	  			InventoryItemAddedToOrder(orderId, inventoryItemId1, 5, 6),
	  			InventoryItemAddedToOrder(orderId, inventoryItemId2, 2, 5),
  				InventoryItemAddedToOrder(orderId, inventoryItemId3, 3, 4),
  				InventoryItemAddedToOrder(orderId, inventoryItemId2, 1, 3),
  				InventoryItemAddedToOrder(orderId, inventoryItemId1, 2, 2),
	  			OrderCreated(orderId, "Test Order", 1)
  			)
	  		
	  		val finalState = Domain.Order(history)

			finalState.items get inventoryItemId1 mustEqual 7.some
			finalState.items get inventoryItemId2 mustEqual 3.some
			finalState.items get inventoryItemId3 mustEqual 3.some
	  	}

	  	"have the correct number of items after a serie of removals" in {

	  		val history = List(
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId1, 2, 9),
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId2, 4, 8),
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId3, 6, 7),
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId2, 1, 6),
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId1, 2, 5),
  				InventoryItemAddedToOrder(orderId, inventoryItemId3, 10, 4),
  				InventoryItemAddedToOrder(orderId, inventoryItemId2, 10, 3),
  				InventoryItemAddedToOrder(orderId, inventoryItemId1, 10, 2),
	  			OrderCreated(orderId, "Test Order", 1)
  			)
	  		
	  		val finalState = Domain.Order(history)

			finalState.items get inventoryItemId1 mustEqual 6.some
			finalState.items get inventoryItemId2 mustEqual 5.some
			finalState.items get inventoryItemId3 mustEqual 4.some
	  	}

	  	"have the correct number of items after a serie of additions and removals" in {

	  		val history = List(
	  			InventoryItemAddedToOrder(orderId, inventoryItemId1, 5, 9),
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId3, 1, 8),
	  			InventoryItemAddedToOrder(orderId, inventoryItemId2, 2, 7),
	  			InventoryItemRemovedFromOrder(orderId, inventoryItemId1, 2, 6),
  				InventoryItemAddedToOrder(orderId, inventoryItemId3, 3, 5),
  				InventoryItemRemovedFromOrder(orderId, inventoryItemId2, 1, 4),
  				InventoryItemAddedToOrder(orderId, inventoryItemId2, 1, 3),
  				InventoryItemAddedToOrder(orderId, inventoryItemId1, 2, 2),
	  			OrderCreated(orderId, "Test Order", 1)
  			)
	  		
	  		val finalState = Domain.Order(history)

			finalState.items get inventoryItemId1 mustEqual 5.some
			finalState.items get inventoryItemId2 mustEqual 2.some
			finalState.items get inventoryItemId3 mustEqual 2.some
	  	}
	}
}