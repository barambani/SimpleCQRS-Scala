package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._

import scalaz._

object DomainStates {

	type EvolvableState[A] = State[A, List[Event]]

	type InventoryItemS = EvolvableState[InventoryItem]
	type OrderS = EvolvableState[Order]

	def zeroEvolvableState[A] = State.state[A, List[Event]](Nil)
	
	def foldStateSeq[A](states: Seq[EvolvableState[A]]): EvolvableState[A] = {

		def mergeStates[A](fs: EvolvableState[A], ps: EvolvableState[A]): EvolvableState[A] = State { 
			(s: A) => {
				lazy val (psS, psT) = ps.run(s)
				lazy val (fsS, fsT) = fs.run(psS)
				(fsS, fsT ::: psT)
			}
		}

		(states foldRight zeroEvolvableState[A]) ((fs,ps) => mergeStates(fs, ps))
	}
}