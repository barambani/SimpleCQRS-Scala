package SimpleCqrsScala.CommandSide.Application

import scalaz.Monad
import scalaz.Kleisli
import scala.language.higherKinds

object EventStoreType {

  type Cassandra = Cassandra.type
  type Redis = Redis.type
 
  sealed trait EventStoreType
  final case object Cassandra extends EventStoreType
  final case object Redis extends EventStoreType
}

import SimpleCqrsScala.CommandSide.Application.EventStoreType._

trait EventStore[S] {
  def read[F[_]](implicit MO: Monad[F]): StoreRetrieve[F]
  def write[F[_]](implicit MO: Monad[F]): StoreInsert[F]
}

object EventStore {
  def apply[S <: EventStoreType](implicit INST: EventStore[S]): EventStore[S] = INST
}

//  ----------------------------------
//  Actual event store implementations
//  ----------------------------------
trait CassandraStore {

  implicit val cassandraEventStore = new EventStore[Cassandra] {
    
    def read[F[_]](implicit MO: Monad[F]): StoreRetrieve[F] =
      Kleisli { id => MO.point { Nil } }

    def write[F[_]](implicit MO: Monad[F]): StoreInsert[F] =
      Kleisli { events => MO.point { () } }
  }
}

trait RedisEventStore {

  implicit val redisEventStore = new EventStore[Redis] {
    
    def read[F[_]](implicit MO: Monad[F]): StoreRetrieve[F] =
      Kleisli { id => MO.point { Nil } }

    def write[F[_]](implicit MO: Monad[F]): StoreInsert[F] =
      Kleisli { events => MO.point { () } }
  }
}
