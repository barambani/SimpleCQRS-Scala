package SimpleCqrsScala.CommandSide.Application

import scalaz.Kleisli
import cats.effect._

object EventStoreType {

  type Cassandra = Cassandra.type
  type Redis = Redis.type
 
  sealed trait EventStoreType
  final case object Cassandra extends EventStoreType
  final case object Redis extends EventStoreType
}

import SimpleCqrsScala.CommandSide.Application.EventStoreType._

trait EventStore[S] {
  def read: StoreRetrieve
  def write: StoreInsert
}

object EventStore {
  def apply[S <: EventStoreType](implicit INST: EventStore[S]): EventStore[S] = INST
}

trait EventStoreTypes {

  implicit val cassandraEventStore = new EventStore[Cassandra] {
    def read: StoreRetrieve =
      Kleisli { id => IO { Nil } }

    def write: StoreInsert =
      Kleisli { events => IO { () } }
  }
}
