package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Errors._
import scalaz.StateT
import scalaz.State
import scalaz.Monad
import scalaz.Reader
import scalaz.{\/, -\/, \/-}
import Validator._

import AggregateRoot._

object DomainState {
	
	type EitherTransition[S]   = StateT[Validated, S, List[Event]]
	
	object EitherTransition {

		def zeroTransition[S: Aggregate] = State.state[S, List[Event]](Nil)

		def apply[S: Aggregate](st: S => Validated[(S, List[Event])]): EitherTransition[S] = 
			StateT[Validated, S, List[Event]](st)


		def liftEvents[S: Aggregate](a: List[Event]): EitherTransition[S] = 
			stateFor(a).lift[Validated]

		def liftEventsF[S: Aggregate](fA: S => List[Event]): EitherTransition[S] = 
			stateForF(fA).lift[Validated]

		def liftValidated[S: Aggregate](ve: Validated[List[Event]]): EitherTransition[S] =
			liftValidatedF(_ => ve)

		def liftValidatedF[S: Aggregate](fVe: S => Validated[List[Event]]): EitherTransition[S] = 
			apply(s => fVe(s) map (stateFor(_).run(s)))

		def liftError[S: Aggregate, ER <: ErrorMessage](e: -\/[ER]): EitherTransition[S] =
			apply(_ => e)
		

		def execTransition[S: Aggregate](eT: EitherTransition[S])(aState: S): \/[ErrorMessage, S] =
			eT.exec(aState)

		def evalTransition[S: Aggregate](eT: EitherTransition[S])(aState: S): \/[ErrorMessage, List[Event]] = 
			eT.eval(aState)


		private def stateFor[S: Aggregate](e: List[Event]): State[S, List[Event]] = for {
			events	<- State.state(e)
			_		<- State.modify[S]{ evolve(_)(events) }

		} yield events

		private def stateForF[S: Aggregate](eF: S => List[Event]): State[S, List[Event]] = for {
			events	<- State.gets(eF)
			_		<- State.modify[S]{ evolve(_)(events) }

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