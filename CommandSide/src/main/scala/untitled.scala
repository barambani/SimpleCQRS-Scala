package SimpleCqrsScala.CommandSide

trait Show[A] {
	def print(x: A): String
}

object Printer {
	def print[A : Show](toPrint: A): String = implicitly[Show[A]] print toPrint
}

object Show {
	implicit def InventoryItemCreatedShow = new Show[InventoryItemCreated] {
		def print(x: InventoryItemCreated): String = "seq: " + x.sequence + " - Item " + x.name + " created (id:" + x.id.toString() + ")"
	}
	implicit def InventoryItemDeactivatedShow = new Show[InventoryItemDeactivated] {
		def print(x: InventoryItemDeactivated): String = "seq: " + x.sequence + " - Item deactivated (id:" + x.id.toString() + ")"
	}
	implicit def InventoryItemRenamedShow = new Show[InventoryItemRenamed] {
		def print(x: InventoryItemRenamed): String = "seq: " + x.sequence + " - Item renamed to " + x.newName + " (id:" + x.id.toString() + ")"
	}
	implicit def ItemsCheckedInToInventoryShow = new Show[ItemsCheckedInToInventory] {
		def print(x: ItemsCheckedInToInventory): String = "seq: " + x.sequence + " - Check-in of " + x.count.toString() + " item/s (id:" + x.id.toString() + ")"
	}
	implicit def ItemsRemovedFromInventoryShow = new Show[ItemsRemovedFromInventory] {
		def print(x: ItemsRemovedFromInventory): String = "seq: " + x.sequence + " - Removed " + x.count.toString() + " item/s (id:" + x.id.toString() + ")"
	}
}