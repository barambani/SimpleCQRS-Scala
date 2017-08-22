package SimpleCqrsScala.CommandSide

import java.util.UUID

import SimpleCqrsScala.CommandSide.Domain.Events._

import scalaz.Kleisli
import scalaz.Bind

import cats.effect._

package object Application {

  type StoreRetrieve = Kleisli[IO, UUID, List[Event]]
  type StoreInsert   = Kleisli[IO, List[Event], Unit]

  type CacheGet[A] = Kleisli[IO, UUID, Option[A]]
  type CachePut[A] = Kleisli[IO, A, Unit]

  implicit object IoBind extends Bind[IO] {
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa map f
  }
}
