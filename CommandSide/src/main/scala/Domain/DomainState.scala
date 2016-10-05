package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._

import scalaz._

object DomainState {

	type StateTransition[A]	= State[A, List[Event]]
	type CommandExecution[A] = A => List[Event]

	def unitTransition[A] = State.state[A, List[Event]](Nil)
	
	def mergeTransitions[A](transitions: Seq[StateTransition[A]]): StateTransition[A] = {

		def mergeSingleTransition[A](t: StateTransition[A], curr: StateTransition[A]): StateTransition[A] = State { 
			(s: A) => {
				lazy val (currS, currT) = curr.run(s)
				lazy val (tS, tT) = t.run(currS)
				(tS, tT ::: currT)
			}
		}

		(transitions foldRight unitTransition[A]) ((t,curr) => mergeSingleTransition(t, curr))
	}

	def execTransition[A]: A => StateTransition[A] => A = st => tr => tr.exec(st)
	def evalTransition[A]: A => StateTransition[A] => List[Event] = st => tr => tr.eval(st)
}