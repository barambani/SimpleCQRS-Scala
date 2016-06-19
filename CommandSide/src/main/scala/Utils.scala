package SimpleCqrsScala.CommandSide

object Printer {
	def print[A : Show](x: A): String = implicitly[Show[A]] stringFor x
}

object MapOps {
	def updateValue[I,A](m: Map[I, A])(i: I)(f: A => A): Map[I, A] = (m - i) + (i -> (f compose m.apply _)(i))
}