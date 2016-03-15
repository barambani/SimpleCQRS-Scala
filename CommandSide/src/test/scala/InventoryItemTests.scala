package InventoryItemTests

import org.specs2.mutable._
import InventoryItem._
import Events._

import java.util.UUID

object InventoryItemSpec extends Specification {

	val id = UUID.randomUUID

	"InventoryItem" should {
		
	  	"have the correct state after the InventoryItemCreated event" in {

	  		val initialState = InventoryItem.initialState
	  		val history = List(InventoryItemCreated(id, "Test Inventory Item"))

	  		val finalState = InventoryItem.evolve(initialState, history)

  			finalState.id mustEqual id
			finalState.name mustEqual "Test Inventory Item"
			finalState.isActivated mustEqual true
	  	}
	}
}