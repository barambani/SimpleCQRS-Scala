package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._

import scalaz._

object DomainStates {

	type StateTransition[A] = State[A, List[Event]]

	def unitTransition[A] = State.state[A, List[Event]](Nil)
	
	def mergeStateTransitions[A](states: Seq[StateTransition[A]]): StateTransition[A] = {

		def mergeStates[A](fs: StateTransition[A], ps: StateTransition[A]): StateTransition[A] = State { 
			(s: A) => {
				lazy val (psS, psT) = ps.run(s)
				lazy val (fsS, fsT) = fs.run(psS)
				(fsS, fsT ::: psT)
			}
		}

		(states foldRight unitTransition[A]) ((fs,ps) => mergeStates(fs, ps))
	}

	def execState[A]: StateTransition[A] => A => A = st => i => st.exec(i)
	def evalState[A]: StateTransition[A] => A => List[Event] = st => i => st.eval(i)
}