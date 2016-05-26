package SimpleCqrsScala.CommandSide

import java.util.UUID

trait Repository {
	def Save(es: List[Event]): Unit 
	def GetHistoryById(id: UUID): List[Event]
}

class EventStore extends Repository {
	def Save(es: List[Event]): Unit = ???
	def GetHistoryById(id: UUID): List[Event] = ???
}