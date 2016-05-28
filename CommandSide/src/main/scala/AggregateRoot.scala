package SimpleCqrsScala.CommandSide

trait Aggregate[A] {
	def getNewStateFor(currentState: A, happens: Event): A
}

object Aggregate {
	implicit lazy val inventoryItemAggregate = new Aggregate[InventoryItem] { 
		def getNewStateFor(currentState: InventoryItem, happens: Event): InventoryItem = currentState getNewStateWhen happens
	}

}

object AggregateRoot {
	def rehydrate[A : Aggregate](initialState: A, history: List[Event]): A = 
		(history foldRight initialState) ((e, s) => implicitly[Aggregate[A]].getNewStateFor(s, e))
}