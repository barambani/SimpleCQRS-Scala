package SimpleCqrsScala.CommandSide.Domain

import scalaz.{\/, -\/, \/-}
import scalaz.NonEmptyList
import scalaz.Applicative
import SimpleCqrsScala.CommandSide.Domain.Errors._

object Validator {

  type Validated[A] = \/[NonEmptyList[ErrorMessage], A]

  def failedWith[A](e: ErrorMessage): Validated[A] = -\/(NonEmptyList(e))
  def succeeded[A](a: A): Validated[A] = \/-(a)
}
