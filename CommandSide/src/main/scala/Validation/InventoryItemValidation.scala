package SimpleCqrsScala.CommandSide.Validation

import SimpleCqrsScala.CommandSide.Domain.InventoryItem
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.Errors._

import java.util.UUID

trait InventoryItemValidation {

	def theNameIsValid(id: UUID, actualName: Option[String])(name: String): Validated[String] = 
		name.isEmpty match {
			case true 	=> failedWith(InventoryItemNameNotValid(id, actualName, name))
			case false 	=> succeeded(name)
		}

	def availableInStock(item: InventoryItem)(count: Int): Validated[Int] = 
		item.itemsCount >= count match {
			case true 	=> succeeded(count)
			case false 	=> failedWith(NotEnoughItemsInStock(item.id, item.name, count))
		}
}