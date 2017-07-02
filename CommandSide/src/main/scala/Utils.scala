package SimpleCqrsScala.CommandSide

object Printer {
	def show[A](x: A)(implicit SH: Show[A]): String = SH stringFor x
}