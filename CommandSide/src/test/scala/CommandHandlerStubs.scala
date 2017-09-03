package SimpleCqrsScala.CommandSide.Test

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._
import SimpleCqrsScala.CommandSide.Application.Handler._
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, InventoryItem, Identity}
import SimpleCqrsScala.CommandSide.Application._
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._

import cats.effect.IO

import scalaz.Monad
import scalaz.Kleisli

import scala.language.higherKinds

trait CommandHandlerStubs {

  lazy val itemId = UUID.randomUUID
  lazy val orderId = UUID.randomUUID

  implicit object IoMonad extends Monad[IO] {
    def point[A](a: => A): IO[A] = IO { a }
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  }
  
  lazy val testStore: Map[UUID, List[Event]] = Map(
    itemId -> List(
      UnknownHappened(itemId, 6),
      ItemsRemovedFromInventory(itemId, 4, 5),
      ItemsRemovedFromInventory(itemId, 3, 4),
      InventoryItemRenamed(itemId, "Second Inventory Item Name", 3),
      ItemsCheckedInToInventory(itemId, 25, 2),
      InventoryItemCreated(itemId, "First Inventory Item Name", 1)
    ),
    orderId -> List(OrderCreated(orderId, "Test Order", 1))
  )

  val orderTestCache = new Cache[LocalActor, Order] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, Order] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, Order] =
      Kleisli { a => MO.point { () } }
  }

  val inventoryItemTestCache = new Cache[LocalActor, InventoryItem] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, InventoryItem] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, InventoryItem] =
      Kleisli { a => MO.point { () } }
  }

  val testEventStore = new EventStore[Cassandra] {
 
    def read[F[_]](implicit MO: Monad[F]): StoreRetrieve[F] =
      Kleisli { id => MO.point { testStore.getOrElse(id, Nil) } }

    def write[F[_]](implicit MO: Monad[F]): StoreInsert[F] =
      Kleisli { events => MO.point { () } }
  }

  implicit val currentTestOrderState: CurrentAggregateState[LocalActor, Cassandra, Order] = 
    new CurrentAggregateState[LocalActor, Cassandra, Order] {
      val CA = orderTestCache
      val ES = testEventStore
      val AGG = Aggregate[Order]
    }

  implicit val currentTestInventoryItemState: CurrentAggregateState[LocalActor, Cassandra, InventoryItem] = 
    new CurrentAggregateState[LocalActor, Cassandra, InventoryItem] {
      val CA = inventoryItemTestCache
      val ES = testEventStore
      val AGG = Aggregate[InventoryItem]
    }
  
  def handleInTest[C <: Command, A <: Identity](command: C)(
    implicit
      H: Handler.AUX[C, A],
      AGG: Aggregate[A],
      CA: CurrentAggregateState[LocalActor, Cassandra, A]): Validated[(A, List[Event])] =
    command.handle[LocalActor, Cassandra, IO].unsafeRunSync
}
