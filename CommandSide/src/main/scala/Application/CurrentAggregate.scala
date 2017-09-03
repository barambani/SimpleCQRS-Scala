package SimpleCqrsScala.CommandSide.Application

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.Aggregate
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._

import scalaz.Monad
import scalaz.Kleisli
import scala.language.higherKinds

sealed trait CurrentAggregateState[A] {

  def fromCacheOrRehydrate[CT <: CacheType, ST <: EventStoreType, F[_]](
    implicit 
      AGG: Aggregate[A],
      CA : Cache[CT, A],
      ES : EventStore[ST],
      MO : Monad[F]): Kleisli[F, UUID, A] =
    CA.read flatMap { 
      mayBeAgg => mayBeAgg.fold(rehydrateFromStore){ 
        agg => Kleisli { _ => MO.point { agg } }
      }
    }

  private def rehydrateFromStore[CT <: CacheType, ST <: EventStoreType, F[_]](
    implicit 
      AGG: Aggregate[A],
      CA : Cache[CT, A],
      ES : EventStore[ST],
      MO : Monad[F]): Kleisli[F, UUID, A] =
    ES.read map { events => AGG.rehydrate(events) }
}

object CurrentAggregateState {
  def apply[A]: CurrentAggregateState[A] = new CurrentAggregateState[A]{}
}
