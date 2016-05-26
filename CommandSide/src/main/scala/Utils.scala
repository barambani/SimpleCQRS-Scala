package SimpleCqrsScala.CommandSide

object Printer {
	def print[A : Show](x: A): String = implicitly[Show[A]] stringFor x
}