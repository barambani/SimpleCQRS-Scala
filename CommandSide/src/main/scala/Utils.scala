package SimpleCqrsScala.CommandSide

object Printer {
	def print[A : Show](toPrint: A): String = implicitly[Show[A]] stringFor toPrint
}