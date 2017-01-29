package SimpleCqrsScala.CommandSide

import scala.annotation.implicitNotFound

object Printer {
	@implicitNotFound("implicit not found for Show[{A}]")
	def print[A](x: A)(implicit SH: Show[A]): String = SH stringFor x
}

object MapOps {
	def updateValue[I,A](m: Map[I, A])(i: I)(f: A => A): Map[I, A] = (m - i) + (i -> (f compose m.apply _)(i))
}