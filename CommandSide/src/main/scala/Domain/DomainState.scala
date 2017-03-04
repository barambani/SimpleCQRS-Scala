package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._
import scalaz.StateT
import scalaz.State
import scalaz.Monad
import scalaz.Reader
import scalaz.{\/, -\/, \/-}
import Validator._

import AggregateRoot._

object DomainState {

	type CommandApplication[S] = Reader[S, List[Event]]
	
	type EitherTransition[S]   = StateT[Validated, S, List[Event]]
	
	object EitherTransition {

		def zeroTransition[S: Aggregate] = State.state[S, List[Event]](Nil)

		def apply[S: Aggregate](es: Validated[(S, List[Event])]): EitherTransition[S] = 
			StateT[Validated, S, List[Event]](_ => es)

		def applyF[S: Aggregate](st: S => Validated[(S, List[Event])]): EitherTransition[S] = 
			StateT[Validated, S, List[Event]](st)


		def liftS[S: Aggregate](ca: CommandApplication[S]): EitherTransition[S] = 
			stateFor(ca).lift[Validated]

		def liftA[S: Aggregate](a: List[Event]): EitherTransition[S] = 
			stateForEvs(a).lift[Validated]

		def liftStoA[S: Aggregate](fA: S => List[Event]): EitherTransition[S] = 
			liftVal(s => \/-(fA(s)))

		def liftE[S: Aggregate](e: ErrorMessage): EitherTransition[S] =
			apply(-\/(e))

		def liftVal[S: Aggregate](fVe: S => Validated[List[Event]]): EitherTransition[S] = 
			applyF(s => fVe(s) map (stateForEvs[S](_).run(s)))

		def liftValPure[S: Aggregate](ve: Validated[List[Event]]): EitherTransition[S] = 
			applyF(s => ve map (stateForEvs[S](_).run(s)))

		
		//	alias for lift state
		def newTransition[S: Aggregate](ca: CommandApplication[S]): EitherTransition[S] = 
			liftS(ca)

		//	alias for lift state value
		def newTransitionA[S: Aggregate](a: List[Event]): EitherTransition[S] = 
			liftA(a)

		//	alias for lift validated state value creator
		def newTransitionV[S: Aggregate](fVe: S => Validated[List[Event]]): EitherTransition[S] = 
			liftVal(fVe)

		//	alias for lift validated state value
		def newTransitionVPure[S: Aggregate](ve: Validated[List[Event]]): EitherTransition[S] = 
			liftValPure(ve)

		//	alias for lift error message
		def failedTransition[S: Aggregate, ER <: ErrorMessage](e: ER): EitherTransition[S] = 
			liftE(e)

		//	alias for lift error message in the form of left side of EitherState
		def failedTransitionL[S: Aggregate, ER <: ErrorMessage](e: -\/[ER]): EitherTransition[S] = 
			apply(e)
		

		def execTransition[S: Aggregate](eT: EitherTransition[S])(aState: S): \/[ErrorMessage, S] =
			eT.exec(aState)

		def evalTransition[S: Aggregate](eT: EitherTransition[S])(aState: S): \/[ErrorMessage, List[Event]] = 
			eT.eval(aState)

		def stateForE[S: Aggregate](event: Event): State[S, List[Event]] =
			stateForEvs(event :: Nil)

		def stateForEvs[S: Aggregate](events: List[Event]): State[S, List[Event]] = for {
			events	<- State.state(events)
			_		<- State.modify { s: S => evolve(s)(events) }

		} yield events


		def stateFor[S: Aggregate](ca: CommandApplication[S]): State[S, List[Event]] = for {
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