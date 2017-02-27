package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.InventoryItem
import SimpleCqrsScala.CommandSide.Domain.Order
import SimpleCqrsScala.CommandSide.Domain._

object AggregateLawsTests extends Specification {

	"InventoryItem" should {
		"should respect the Aggregate laws" in {
  			AggregateLaws.law1[InventoryItem] must be equalTo(true)
  			AggregateLaws.law2[InventoryItem] must be equalTo(true)
		}
	}

	"Order" should {
	  	"should respect the Aggregate laws" in {
  			AggregateLaws.law1[Order] must be equalTo(true)
  			AggregateLaws.law2[Order] must be equalTo(true)
	  	}
	}
}