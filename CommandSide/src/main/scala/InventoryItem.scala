package InventoryItem

import Domain._
import Events._
import java.util.UUID

object InventoryItem {
	def apply(id: UUID, name: String): InventoryItem = Domain.evolve(new InventoryItem(), InventoryItemCreated(id, name))
}

class InventoryItem private () extends AggregateRoot {

	//	State
	private var _id: Option[UUID] = None
	private var _name: Option[String] = None
	private var _isAactivated: Option[Boolean] = None
	
	override def changeStateAsHappened(event: Event): Unit = event match {
		
		case InventoryItemCreated(id, name) => {
			_id = Some(id)
			_name = Some(name)
			_isAactivated = Some(true)
		}
		case InventoryItemDeactivated(id) => _isAactivated = Some(false)
		case _ => _isAactivated = _isAactivated
	}
}