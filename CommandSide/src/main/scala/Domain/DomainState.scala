package SimpleCqrsScala.CommandSide.Domain

import SimpleCqrsScala.CommandSide._
import scalaz.StateT
import scalaz.State
import scalaz.Monad
import scalaz.{\/, -\/, \/-}

import AggregateRoot._

object DomainState {

	type EitherState[S] = \/[ErrorMessage, S]
	type EitherTransition[S] = StateT[EitherState, S, List[Event]]

	type CommandExecution[S] = S => List[Event]

	def zeroTransition[S] = State.state[S, List[Event]](Nil)

	def liftS[S: Aggregate](f: S => (S, List[Event])): EitherTransition[S] = 
		StateT[EitherState, S, List[Event]]{ s => Monad[EitherState].point(f(s)) }

	def liftE[S](e: ErrorMessage): EitherTransition[S] =
		StateT[EitherState, S, List[Event]]{ _ => -\/(e) }
	
	def newTransition[S : Aggregate](ce: CommandExecution[S]): EitherTransition[S] =
		liftS[S]{ s => (evolve(s)(ce(s)), ce(s)) }

	def failedTransition[S](e: ErrorMessage): EitherTransition[S] =
		liftE(e)
	
	def execTransition[S]: EitherTransition[S] => S => EitherState[S] =
		eitherTransition => aState => eitherTransition.exec(aState)

	def evalTransition[S]: EitherTransition[S] => S => \/[ErrorMessage, List[Event]] = 
		eitherTransition => aState => eitherTransition.eval(aState)


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