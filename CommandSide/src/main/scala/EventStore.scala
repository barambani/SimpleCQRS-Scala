package SimpleCqrsScala.CommandSide.EventStore

import java.util.UUID

import SimpleCqrsScala.CommandSide.Events._

sealed trait Repository {
	def Save(e: Event): Unit
	def Save(es: List[Event]): Unit 
	def GetHistoryById(id: UUID): List[Event]
}

class EventStore extends Repository {

	def Save(e: Event): Unit = ???

	def Save(es: List[Event]): Unit = ???

	def GetHistoryById(id: UUID): List[Event] = ???
}