package SimpleCqrsScala.CommandSide.Domain

import scalaz.{\/, -\/, \/-}
import scalaz.Applicative
import SimpleCqrsScala.CommandSide.Domain.Errors._

object Validator {
	
	type Validated[A] = \/[ErrorMessage, A]

	lazy val validation = Applicative[Validated]
}