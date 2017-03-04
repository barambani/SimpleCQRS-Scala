package SimpleCqrsScala.CommandSide.Domain

import scalaz.{\/, -\/, \/-}
import scalaz.Applicative

object Validator {
	
	type Validated[A] = \/[ErrorMessage, A]

	lazy val validation = Applicative[Validated]
}