package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._
import MapOps._

object OrderItems {

	type OrderItems = Map[UUID, Int]

	lazy val empty: OrderItems = Map.empty

	def canRemoveTheItem: OrderItems => UUID => Int => Boolean = 
		oi => itemId => quantity => (oi get itemId).fold(false){ _ >= quantity }

	def addToItems: OrderItems => UUID => Int => OrderItems =
		oi => itemId => quantity =>	if(oi contains itemId) updateValue(oi)(itemId)(ei => ei + quantity)
									else oi + (itemId -> quantity)

	def removeFromItems: OrderItems => UUID => Int => OrderItems =
		oi => itemId => quantity =>	if(oi contains itemId) updateValue(oi)(itemId)(ei => ei - quantity)
									else oi // TODO: May be error? trying to remove unexisting item
}