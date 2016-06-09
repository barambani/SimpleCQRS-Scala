package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._

import scalaz._

object DomainTypes {

	type EvolvableState[A] = State[A, List[Event]]

	type InventoryItemS = EvolvableState[InventoryItem]
	type OrderS = EvolvableState[Order]

	def mergeStates[A](fs: EvolvableState[A], ps: EvolvableState[A]): EvolvableState[A] = State { 
		(s: A) => {
			lazy val (psS, psT) = ps.run(s)
			lazy val (fsS, fsT) = fs.run(psS)
			(fsS, fsT ::: psT)
		}
	}
	
	def getFinalState[A](states: Seq[EvolvableState[A]]): EvolvableState[A] = {
		
		def iterate(state: EvolvableState[A], xs: Seq[EvolvableState[A]]): EvolvableState[A] =
			if(xs.isEmpty) state
			else iterate(mergeStates(xs.head, state), xs.tail)

		iterate(State.state(Nil), states)
	}
}