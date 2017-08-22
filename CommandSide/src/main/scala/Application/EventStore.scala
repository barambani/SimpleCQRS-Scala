package SimpleCqrsScala.CommandSide.Application

import scalaz.Kleisli
import cats.effect._

sealed trait EventStoreType
sealed trait Cassandra extends EventStoreType
sealed trait Redis extends EventStoreType

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
