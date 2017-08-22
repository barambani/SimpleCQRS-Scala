package SimpleCqrsScala.CommandSide.Test

import org.specs2.mutable._
import SimpleCqrsScala.CommandSide.Domain.Order
import SimpleCqrsScala.CommandSide.Domain.Order._
import SimpleCqrsScala.CommandSide.Domain.DomainState.EitherTransition._
import SimpleCqrsScala.CommandSide.Services.OrderService
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._
import SimpleCqrsScala.CommandSide.{OrderEventsShow, ErrorsShow}
import SimpleCqrsScala.CommandSide.Show.ShowSyntax

import java.util.UUID

object OrderServiceTests extends Specification with OrderService with ErrorsShow with OrderEventsShow {

  lazy val orderId = UUID.randomUUID
  lazy val customerId = UUID.randomUUID

  lazy val initialState: Validated[Order] = 
    execTransition(createOrderFor(orderId, customerId, "Test Customer"))(Order.empty)

  "An order service" should {

    "return an error trying to add an empty shipping address to the order" in {

      lazy val newState = initialState flatMap execTransition(addShippingAddressToOrder(""))

      newState.fold (
        es  => es.show mustEqual s"the shipping address '' proposed for the order 'Order for Test Customer (id: ${customerId})' (id: ${orderId}) is not valid",
        _   => ko("The order shouldn't accept empty shipping address") 
      )
    }
  }
}
