package SimpleCqrsScala.CommandSide

import SimpleCqrsScala.CommandSide.Domain._
import java.util.UUID

trait Repository {
	def Save(es: List[Event]): Unit 
	def GetHistoryById(id: UUID): List[Event]
}

object EventStore extends Repository {
	def Save(es: List[Event]): Unit = ???
	def GetHistoryById(id: UUID): List[Event] = ???
}