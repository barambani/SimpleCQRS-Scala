package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._
import scalaz.StateT
import scalaz.EitherT
import scalaz.State
import scalaz.\/

import AggregateRoot._

object DomainState {

	type StateTransition[A] = State[A, List[Event]]

	type EitherTransition[A] = EitherT[StateTransition, ErrorMessage, A]

	type CommandExecution[A] = A => List[Event]

	def zeroTransition[A] = State.state[A, List[Event]](Nil)

	def newTransition[A : Aggregate](ce: CommandExecution[A]): EitherTransition[A] = ???
		// for {
		// 	es 	<- State gets ce 
		// 	_ 	<- State modify { s: A => evolve(s)(es) }
		// } yield es

	def failedTransition[A : Aggregate](e: ErrorMessage): EitherTransition[A] = ???
	
	def mergeTransitions[A](transitions: Seq[StateTransition[A]]): StateTransition[A] = {

		def mergeSingleTransition[A](t: StateTransition[A], curr: StateTransition[A]): StateTransition[A] = State { 
			(s: A) => {
				lazy val (currS, currT) = curr.run(s)
				lazy val (tS, tT) = t.run(currS)
				(tS, tT ::: currT)
			}
		}

		(transitions foldRight zeroTransition[A]) ((t,curr) => mergeSingleTransition(t, curr))
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