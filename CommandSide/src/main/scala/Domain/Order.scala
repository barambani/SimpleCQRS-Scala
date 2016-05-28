package SimpleCqrsScala.CommandSide.Domain

import java.util.UUID
import SimpleCqrsScala.CommandSide._

object Order {
	def apply(history: List[Event]): Order = AggregateRoot.evolve(new Order, history)
}

class Order private (
	val id: UUID = new UUID(0, 0),
	val customer: Customer = Customer(new UUID(0, 0), ""),
	val version: Long = 0) extends Identity with Versioned {

	def getNewStateWhen(event: Event): Order = ???

}

case class Customer(val id: UUID, val name: String) extends Identity