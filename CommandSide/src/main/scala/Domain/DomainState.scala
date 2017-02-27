package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._
import scalaz.StateT
import scalaz.State
import scalaz.Monad
import scalaz.Reader
import scalaz.{\/, -\/, \/-}

import AggregateRoot._

object DomainState {

	type EitherState[S] 	   = \/[ErrorMessage, S]
	type CommandApplication[S] = Reader[S, List[Event]]
	
	type EitherTransition[S]   = StateT[EitherState, S, List[Event]]
	
	object EitherTransition {

		def zeroTransition[S: Aggregate] = State.state[S, List[Event]](Nil)

		def apply[S: Aggregate](es: EitherState[(S, List[Event])]): EitherTransition[S] = 
			StateT[EitherState, S, List[Event]](_ => es)

		def liftS[S: Aggregate](ca: CommandApplication[S]): EitherTransition[S] = 
			stateFor(ca).lift[EitherState]

		def liftE[S: Aggregate](e: ErrorMessage): EitherTransition[S] =
			apply(-\/(e))
		
		//	alias for lift state
		def newTransition[S: Aggregate](ca: CommandApplication[S]): EitherTransition[S] = 
			liftS(ca)

		//	alias for lift error message
		def failedTransition[S: Aggregate, ER <: ErrorMessage](e: ER): EitherTransition[S] = 
			liftE(e)
		
		def execTransition[S: Aggregate](eT: EitherTransition[S])(aState: S): \/[ErrorMessage, S] =
			eT.exec(aState)

		def evalTransition[S: Aggregate](eT: EitherTransition[S])(aState: S): \/[ErrorMessage, List[Event]] = 
			eT.eval(aState)

		private def stateFor[S: Aggregate](ca: CommandApplication[S]): State[S, List[Event]] = for {
			events	<- State.gets(ca.run)
			_		<- State.modify { s: S => evolve(s)(events) }

		} yield events
	}

	// def mergeTransitions[A](transitions: Seq[StateTransition[A]]): StateTransition[A] = {

	// 	def mergeSingleTransition[A](t: StateTransition[A], curr: StateTransition[A]): StateTransition[A] = State { 
	// 		(s: A) => {
	// 			lazy val (currS, currT) = curr.run(s)
	// 			lazy val (tS, tT) = t.run(currS)
	// 			(tS, tT ::: currT)
	// 		}
	// 	}

	// 	(transitions foldRight zeroTransition[A]) ((t,curr) => mergeSingleTransition(t, curr))
	// }


	// def execTransitions[A]: Seq[StateTransition[A]] => A => A = 
	// 	trns => st => mergeTransitions(trns) exec st

	// def evalTransitions[A]: Seq[StateTransition[A]] => A => List[Event] = 
	// 	trns => st => mergeTransitions(trns) eval st
}