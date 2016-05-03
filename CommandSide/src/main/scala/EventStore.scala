package SimpleCqrsScala.CommandSide.EventStore

import java.util.UUID

import SimpleCqrsScala.CommandSide.Events._

trait Repository {
	def Save(es: List[Event]): Unit 
	def GetHistoryById(id: UUID): List[Event]
}

class EventStore extends Repository {
	def Save(es: List[Event]): Unit = ???
	def GetHistoryById(id: UUID): List[Event] = ???
}