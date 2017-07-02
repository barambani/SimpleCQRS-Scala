package SimpleCqrsScala.CommandSide.Application

import java.util.UUID
import scalaz.ReaderT
import cats.effect._

import SimpleCqrsScala.CommandSide.Domain.Events._

trait Cache[A] {
	type CacheGet[A] = ReaderT[IO, UUID, A]
	type CachePut[A] = ReaderT[IO, A, Unit]

	def read: CacheGet[A]
	def write: CachePut[A]
}