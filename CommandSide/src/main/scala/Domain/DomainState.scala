package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._
import SimpleCqrsScala.CommandSide.Domain.Events._
import SimpleCqrsScala.CommandSide.Domain.Errors._
import scalaz.StateT
import scalaz.State
import scalaz.Monad
import scalaz.Reader
import scalaz.{\/, -\/, \/-}
import scalaz.NonEmptyList
import Validator._

import AggregateRoot._

object DomainState {
	
	type EitherTransition[S] = StateT[Validated, S, List[Event]]
	
	object EitherTransition {

		def zeroTransition[S](implicit A: Aggregate[S]): State[S, List[Event]] = 
			State.state[S, List[Event]](Nil)

		def apply[S](st: S => Validated[(S, List[Event])])(implicit A: Aggregate[S]): EitherTransition[S] = 
			StateT[Validated, S, List[Event]](st)

		def liftEvents[S](a: List[Event])(implicit A: Aggregate[S]): EitherTransition[S] = 
			stateFor(a).lift[Validated]

		def liftEventsF[S](fA: S => List[Event])(implicit A: Aggregate[S]): EitherTransition[S] = 
			stateForF(fA).lift[Validated]

		def liftValidated[S](ve: Validated[List[Event]])(implicit A: Aggregate[S]): EitherTransition[S] =
			liftValidatedF(_ => ve)

		def liftValidatedF[S](fVe: S => Validated[List[Event]])(implicit A: Aggregate[S]): EitherTransition[S] = 
			apply(s => fVe(s) map (stateFor(_).run(s)))
		
		def execTransition[S](eT: EitherTransition[S])(aState: S)(implicit A: Aggregate[S]): Validated[S] =
			eT.exec(aState)

		def evalTransition[S](eT: EitherTransition[S])(aState: S)(implicit A: Aggregate[S]): Validated[List[Event]] = 
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
}