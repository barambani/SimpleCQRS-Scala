package SimpleCqrsScala.CommandSide.Test

import scalaz.{\/-}
import org.scalacheck._
import Arbitrary.arbitrary

import SimpleCqrsScala.CommandSide.Domain.Validator._

object ArbitraryImplicits {
  implicit def ArbitraryValidated[A](implicit AR: Arbitrary[A]) = Arbitrary[Validated[A]] {
    for(a <- Arbitrary.arbitrary[A]) yield \/-(a)
  }
}
