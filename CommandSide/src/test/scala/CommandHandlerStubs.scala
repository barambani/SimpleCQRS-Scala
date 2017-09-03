package SimpleCqrsScala.CommandSide.Test

import java.util.UUID
import SimpleCqrsScala.CommandSide.Domain.Commands._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Application.CacheType._
import SimpleCqrsScala.CommandSide.Application.EventStoreType._
import SimpleCqrsScala.CommandSide.Application.Handler._
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.{Aggregate, Order, InventoryItem, Identity}
import SimpleCqrsScala.CommandSide.Application.{CacheGet, CachePut, Cache, StoreRetrieve, StoreInsert, EventStore, Handler}

import cats.effect.IO

import scalaz.Monad
import scalaz.Kleisli

import scala.language.higherKinds

trait CommandHandlerStubs {

  lazy val itemId = UUID.randomUUID
  lazy val orderId = UUID.randomUUID

  private implicit object IoMonad extends Monad[IO] {
    def point[A](a: => A): IO[A] = IO { a }
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  }
  
  private lazy val testStore: Map[UUID, List[Event]] = Map(
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

  protected implicit val orderTestCache = new Cache[LocalActor, Order] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, Order] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, Order] =
      Kleisli { a => MO.point { () } }
  }

  protected implicit val inventoryItemTestCache = new Cache[LocalActor, InventoryItem] {

    def read[F[_]](implicit MO: Monad[F]): CacheGet[F, InventoryItem] = 
      Kleisli { id => MO.point { None } }

    def write[F[_]](implicit MO: Monad[F]): CachePut[F, InventoryItem] =
      Kleisli { a => MO.point { () } }
  }

  protected implicit val testEventStore = new EventStore[Cassandra] {
 
    def read[F[_]](implicit MO: Monad[F]): StoreRetrieve[F] =
      Kleisli { id => MO.point { testStore.getOrElse(id, Nil) } }

    def write[F[_]](implicit MO: Monad[F]): StoreInsert[F] =
      Kleisli { events => MO.point { () } }
  }
  
  def handleInTest[C <: Command, A <: Identity](command: C)(
    implicit
      H  : Handler.AUX[C, A],
      CA : Cache[LocalActor, A],
      AGG: Aggregate[A]): Validated[(A, List[Event])] =
    command.handle[LocalActor, Cassandra, IO].unsafeRunSync
}
