package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.InventoryItem
import SimpleCqrsScala.CommandSide.Domain.Order
import SimpleCqrsScala.CommandSide.Domain._
import org.scalacheck._

abstract class AggreagateLaws[A](name: String)(implicit AGG: Aggregate[A]) extends Specification {

	s"$name" should {
		"respect the first Aggregate law: rehydrating from an empty history gives the zero aggregate" in {
  			AggregateLaws.law1[A] must be equalTo(true)
		}

		"respect the second Aggregate law: rehydrating from the sole zero event gives the zero aggregate" in {
  			AggregateLaws.law2[A] must be equalTo(true)
		}

		"respect the third Aggregate law: the new state from the zero aggregate after the zero event is the zero aggregate" in {
  			AggregateLaws.law3[A] must be equalTo(true)
		}
	}
}

object InventoryItemLawsTests extends AggreagateLaws[InventoryItem]("InventoryItem")
object OrderLawsTests extends AggreagateLaws[Order]("Order")
