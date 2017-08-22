package SimpleCqrsScala.CommandSide.Services

import SimpleCqrsScala.CommandSide.Domain.Order
import SimpleCqrsScala.CommandSide.Domain.Open
import SimpleCqrsScala.CommandSide.Domain.Validator._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.DomainState._
import SimpleCqrsScala.CommandSide.Domain.DomainState.EitherTransition._
import SimpleCqrsScala.CommandSide.Domain.DomainAggregates._
import SimpleCqrsScala.CommandSide.Validation.OrderValidation
import scalaz.syntax.applicative._

import java.util.UUID

trait OrderService extends OrderValidation {

  def createOrderFor(id: UUID, customerId: UUID, customerName: String): EitherTransition[Order] =
    liftValidatedF {
      ord => canBeCreated(ord) map { 
        _ => OrderCreated(id, s"Order for $customerName (id: $customerId)", 1) :: Nil
      }
    }

  def addInventoryItemToOrder(itemId: UUID, quantity: Int): EitherTransition[Order] = 
    liftValidatedF {
      ord => canBeChanged(ord) map { 
        _ => InventoryItemAddedToOrder(ord.id, itemId, quantity, ord.expectedNextVersion) :: Nil
      }
    }

  def removeInventoryItemFromOrder(itemId: UUID, quantity: Int): EitherTransition[Order] = 
    liftValidatedF {
      ord => (canBeChanged(ord) |@| hasEnoughItems(ord)(itemId, quantity)) { 
        (_, _) => InventoryItemRemovedFromOrder(ord.id, itemId, quantity, ord.expectedNextVersion) :: Nil
      }
    }

  def addShippingAddressToOrder(address: String): EitherTransition[Order] =
    liftValidatedF {
      ord => (canBeChanged(ord) |@| shippingAddressValid(ord)(address)) { 
        (_, _) => ShippingAddressAddedToOrder(ord.id, address, ord.expectedNextVersion) :: Nil
      }
    }

  def payTheBalance: EitherTransition[Order] =
    liftValidatedF {
      ord => (canBeChanged(ord) |@| canBePayed(ord)) { 
        (_, _) => OrderPayed(ord.id, ord.expectedNextVersion) :: Nil
      }
    }

  def submit: EitherTransition[Order] = 
    liftValidatedF {
      ord => (canBeChanged(ord) |@| containsItems(ord) |@| isPaymentValid(ord)) { 
        (_, _, _) => OrderSubmitted(ord.id, ord.expectedNextVersion) :: Nil
      }
    }
}
