package SimpleCqrsScala.CommandSide

import SimpleCqrsScala.CommandSide.Domain._
import java.util.UUID
import scalaz.ReaderT
import scalaz.concurrent.Task

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