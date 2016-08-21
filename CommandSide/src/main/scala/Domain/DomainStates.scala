package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._

import scalaz._

object DomainStates {

	import InventoryItemOps._
	import OrderOps._

	type EvolvableState[A] = State[A, List[Event]]

	type InventoryItemS = EvolvableState[InventoryItem]
	type OrderS = EvolvableState[Order]

	def zeroEvolvableState[A] = State.state[A, List[Event]](Nil)
	
	def mergeStateTransitions[A](states: Seq[EvolvableState[A]]): EvolvableState[A] = {

		def mergeStates[A](fs: EvolvableState[A], ps: EvolvableState[A]): EvolvableState[A] = State { 
			(s: A) => {
				lazy val (psS, psT) = ps.run(s)
				lazy val (fsS, fsT) = fs.run(psS)
				(fsS, fsT ::: psT)
			}
		}

		(states foldRight zeroEvolvableState[A]) ((fs,ps) => mergeStates(fs, ps))
	}

	lazy val execState: EvolvableState[InventoryItem] => InventoryItem => InventoryItem =
		st => i => st.exec(i)

	lazy val evalState: EvolvableState[InventoryItem] => InventoryItem => List[Event] = 
		st => i => st.eval(i)
}