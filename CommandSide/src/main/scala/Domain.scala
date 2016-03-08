package Domain

import java.util.UUID
import Events._

object Domain {

	def evolve[T <: AggregateRoot](initialState: T, historyStep: Event): T = evolve(initialState, List(historyStep))

	def evolve[T <: AggregateRoot](initialState: T, history: List[Event]): T = {

		def evolveOneStep(state: T, e: Event) = {
			state afterHappened e
			state
		}

		(history foldLeft initialState)((s, e) => evolveOneStep(s, e))
	}
}

abstract class AggregateRoot() {
	
	private var changes: List[Event] = List[Event]()
	
	def getUncommittedChanges: List[Event] = changes
	
	def markChangesAsCommitted: Unit = (changes = List())

	def afterHappened(event: Event): Unit = {
		changeStateAsHappened(event)
		changes = event :: changes
	}

	protected def changeStateAsHappened(e: Event): Unit
}	