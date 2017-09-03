package SimpleCqrsScala.CommandSide.Test

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scalaz.Applicative
import scalaz.Equal
import org.scalacheck._
import scalaz._
import scalaz.Scalaz._

import SimpleCqrsScala.CommandSide.Domain.Validator._

import scala.language.higherKinds
import EqualImplicits._
import ArbitraryImplicits._

abstract class ApplicativeLawsTests[F[_]](name: String)(
  implicit 
    AF:	 Applicative[F], 
    EQI: Equal[F[Int]], 
    EQS: Equal[F[String]],
    AI:	 Arbitrary[Int],
    AFI: Arbitrary[F[Int]],
    AIS: Arbitrary[F[Int => String]]) extends Properties(s"Applicative[$name]") {

  property("`point(identity)` is a no-op") = forAll {
    (fa: F[Int]) => AF.applicativeLaw.identityAp(fa)
  }

  property("`point` distributes over function applications") = forAll {
    (ab: Int => String, a: Int) => AF.applicativeLaw.homomorphism(ab, a)
  }

  property("`point` is a left and right identity, F-wise") = forAll {
    (f: F[Int => String], a: Int) => AF.applicativeLaw.interchange(f, a)
  }

  property("`map` is like the one derived from `point` and `ap`") = forAll {
    (f: Int => String, fa: F[Int]) => AF.applicativeLaw.mapLikeDerived(f, fa)
  }
}

object ValidatorLawsTests extends ApplicativeLawsTests[Validated]("Validated")
