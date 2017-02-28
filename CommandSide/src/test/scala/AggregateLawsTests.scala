package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.InventoryItem
import SimpleCqrsScala.CommandSide.Domain.Order
import SimpleCqrsScala.CommandSide.Domain._
import AggregateLaws._

abstract class AggreagateLawsChecks[A](name: String)(implicit AGG: Aggregate[A]) extends Specification {

	lazy val laws = AggregateLaws[A]

	s"$name" should {
		"respect the first Aggregate law: rehydrating from an empty history gives the zero aggregate" in {
  			laws.law1 must be equalTo(true)
		}

		"respect the second Aggregate law: rehydrating from the sole zero event gives the zero aggregate" in {
  			laws.law2 must be equalTo(true)
		}

		"respect the third Aggregate law: the new state from the zero aggregate after the zero event is the zero aggregate" in {
  			laws.law3 must be equalTo(true)
		}
	}
}

object InventoryItemLawsTests extends AggreagateLawsChecks[InventoryItem]("InventoryItem")
object OrderLawsTests extends AggreagateLawsChecks[Order]("Order")
