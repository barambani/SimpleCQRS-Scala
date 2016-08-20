package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.DomainStates._

object OrderItems {

	import MapOps._

	type OrderItems = Map[UUID, Int]

	lazy val empty: Map[UUID, Int] = Map.empty

	def canRemoveTheItem(oi: OrderItems, itemId: UUID, quantity: Int): Boolean = (oi getOrElse (itemId, 0)) >= quantity

	def addToItems(oi: OrderItems, itemId: UUID, quantity: Int): OrderItems =
		if(oi contains itemId) updateValue(oi)(itemId)(ei => ei + quantity)
		else oi + (itemId -> quantity)

	def removeFromItems(oi: OrderItems, itemId: UUID, quantity: Int): OrderItems =
		if(oi contains itemId) updateValue(oi)(itemId)(ei => ei - quantity)
		else oi // TODO: May be error? trying to remove unexisting item

}