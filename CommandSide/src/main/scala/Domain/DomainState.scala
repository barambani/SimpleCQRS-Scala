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

	def execTransition[A]: StateTransition[A] => A => A =
		tr => st => tr.exec(st)

	def evalTransition[A]: StateTransition[A] => A => List[Event] = 
		tr => st => tr.eval(st)

	def execTransitions[A]: Seq[StateTransition[A]] => A => A = 
		trns => st => mergeTransitions(trns) exec st

	def evalTransitions[A]: Seq[StateTransition[A]] => A => List[Event] = 
		trns => st => mergeTransitions(trns) eval st
}