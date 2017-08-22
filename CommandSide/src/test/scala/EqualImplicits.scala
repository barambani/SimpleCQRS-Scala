package SimpleCqrsScala.CommandSide.Test

import scalaz.Equal
import scalaz.{\/-, -\/}

import SimpleCqrsScala.CommandSide.Domain.Validator._

object EqualImplicits {
  implicit def EqualValidated[A](implicit Eq: Equal[A]) = new Equal[Validated[A]] {
    override def equal(a1: Validated[A], a2: Validated[A]): Boolean = 
      (a1, a2) match {
        case (\/-(x1), \/-(x2))	=> Eq.equal(x1, x2)
        case (-\/(e1), -\/(e2))	=> e1 == e2
        case _                  => false
      }
  }
}
