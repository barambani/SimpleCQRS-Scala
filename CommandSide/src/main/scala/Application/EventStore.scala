package SimpleCqrsScala.CommandSide.Application

import SimpleCqrsScala.CommandSide.Domain._
import java.util.UUID
import scalaz.ReaderT
import scalaz.concurrent.Task
import SimpleCqrsScala.CommandSide.Domain.Events._

object Repository {

	type Query = ReaderT[Task, UUID, List[Event]]
	type Write = ReaderT[Task, List[Event], Unit]

	sealed trait EventStore {
		val sink: Write
		val history: Query
	}
	implicit object EventStore {
		val sink: Write = ???
		val history: Query = ???
	}
}