package SimpleCqrsScala.CommandSide.Application

import java.util.UUID
import scalaz.ReaderT
import cats.effect._
import SimpleCqrsScala.CommandSide.Domain.Events._

trait EventStore {
	type StoreRetrieve = ReaderT[IO, UUID, List[Event]]
	type StoreInsert   = ReaderT[IO, List[Event], Unit]

	def read: StoreRetrieve
	def write: StoreInsert
}
object AnEventStore extends EventStore {
	def read: StoreRetrieve = ???
	def write: StoreInsert  = ???
}
