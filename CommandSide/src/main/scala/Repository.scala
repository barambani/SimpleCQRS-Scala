package SimpleCqrsScala.CommandSide.Repository

import java.util.UUID

import SimpleCqrsScala.CommandSide.Events._

trait Repository {
	def Save(e: Event): Unit
	def Save(es: List[Event]): Unit 
	def GetHistoryById(id: UUID): List[Event]
}