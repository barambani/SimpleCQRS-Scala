package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._

import scalaz._

object DomainState {

	type StateTransition[A] = State[A, List[Event]]
	type CommandExecution[A] = A => List[Event]

	def unitTransition[A] = State.state[A, List[Event]](Nil)
	
	def applyTransitions[A](transitions: Seq[StateTransition[A]]): StateTransition[A] = {

		def applySingleTransition[A](t: StateTransition[A], curr: StateTransition[A]): StateTransition[A] = State { 
			(s: A) => {
				lazy val (currS, currT) = curr.run(s)
				lazy val (tS, tT) = t.run(currS)
				(tS, tT ::: currT)
			}
		}

		(transitions foldRight unitTransition[A]) ((t,curr) => applySingleTransition(t, curr))
	}

	def execTransition[A]: StateTransition[A] => A => A = st => i => st.exec(i)
	def evalTransition[A]: StateTransition[A] => A => List[Event] = st => i => st.eval(i)
}