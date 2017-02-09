package SimpleCqrsScala.CommandSide

import SimpleCqrsScala.CommandSide.Domain._
import java.util.UUID
import scalaz.Kleisli
import fs2.Task

object Repository {

	sealed trait EventStore {
		val sink: Kleisli[Task, List[Event], Unit]
		val history: Kleisli[Task, UUID, List[Event]]
	}
	implicit object EventStore {
		val sink: Kleisli[Task, List[Event], Unit] = ???
		val history: Kleisli[Task, UUID, List[Event]] = ???
	}
}