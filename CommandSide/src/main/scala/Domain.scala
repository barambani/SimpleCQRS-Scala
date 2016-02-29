package Domain

import java.util.UUID
import Events._

abstract class AggregateRoot(id: UUID) {
	
	private var changes: List[Event] = List[Event]()
	
	def GetUncommittedChanges: List[Event] = changes
	def MarkChangesAsCommitted: Unit = (changes = List())
	def applyChange(e: Event): Unit = {
		apply(e)
		changes = e :: changes
	}

	def apply(e: Event): Unit
}