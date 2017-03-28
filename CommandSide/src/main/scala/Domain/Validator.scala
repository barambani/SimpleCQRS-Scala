package SimpleCqrsScala.CommandSide.Domain

import scalaz.{\/, -\/, \/-}
import scalaz.Applicative
import SimpleCqrsScala.CommandSide.Domain.Errors._

object Validator {
	
	type Validated[A] = \/[ErrorMessage, A]

	sealed trait Validation extends Applicative[Validated] {

		def point[A](a: => A): Validated[A] = \/-(a)
		
		def ap[A, B](fa: => Validated[A])(f: => Validated[A => B]): Validated[B] =
			fa flatMap { a => f map { ff => ff(a) } }
	}

	lazy val validation: Applicative[Validated] = new Validation {}
}