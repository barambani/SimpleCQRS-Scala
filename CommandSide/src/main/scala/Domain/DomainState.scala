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
	type EitherTransition[S]   = StateT[EitherState, S, List[Event]]
	type CommandApplication[S] = Reader[S, List[Event]]

	def zeroTransition[S: Aggregate] = State.state[S, List[Event]](Nil)

	def liftS[S: Aggregate](ca: CommandApplication[S]): EitherTransition[S] = 
		StateT[EitherState, S, List[Event]]{ s => ca map (es => Monad[EitherState] point (evolve(s)(es), es)) run s }

	def liftE[S: Aggregate](e: ErrorMessage): EitherTransition[S] =
		StateT[EitherState, S, List[Event]]{ _ => -\/(e) }
	
	//	alias for lift state
	def newTransition[S: Aggregate](ca: CommandApplication[S]): EitherTransition[S] = liftS[S](ca)

	//	alias for lift error message
	def failedTransition[S: Aggregate](e: ErrorMessage): EitherTransition[S] = liftE(e)
	
	def execTransition[S: Aggregate]: EitherTransition[S] => S => EitherState[S] =
		eT => aState => eT.exec(aState)

	def evalTransition[S: Aggregate]: EitherTransition[S] => S => \/[ErrorMessage, List[Event]] = 
		eT => aState => eT.eval(aState)

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