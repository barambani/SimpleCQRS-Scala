package SimpleCqrsScala.CommandSide

import java.util.UUID

object Order {
	private lazy val InitialState = new Order(new UUID(0, 0))
	def apply(history: List[Event]): Order = AggregateRoot.evolve(InitialState, history)
}

class Order private (val id: UUID, val version: Long = 0) extends Identity with Versioned {

	def getNewStateWhen(event: Event): Order = ???

}