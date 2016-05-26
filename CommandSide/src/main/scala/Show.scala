package SimpleCqrsScala.CommandSide

trait Show[A] {
	def stringFor(x: A): String
}

object Show {
	implicit def InventoryItemCreatedShow = new Show[InventoryItemCreated] {
		def stringFor(x: InventoryItemCreated): String = s"seq: ${x.sequence} - Item ${x.name} created (id:${x.id})"
	}
	implicit def InventoryItemDeactivatedShow = new Show[InventoryItemDeactivated] {
		def stringFor(x: InventoryItemDeactivated): String = s"seq: ${x.sequence} - Item deactivated (id:${x.id})"
	}
	implicit def InventoryItemRenamedShow = new Show[InventoryItemRenamed] {
		def stringFor(x: InventoryItemRenamed): String = s"seq: ${x.sequence} - Item renamed to ${x.newName} (id:${x.id})"
	}
	implicit def ItemsCheckedInToInventoryShow = new Show[ItemsCheckedInToInventory] {
		def stringFor(x: ItemsCheckedInToInventory): String = s"seq: ${x.sequence} - Check-in of ${x.count} item/s (id:${x.id})"
	}
	implicit def ItemsRemovedFromInventoryShow = new Show[ItemsRemovedFromInventory] {
		def stringFor(x: ItemsRemovedFromInventory): String = s"seq: ${x.sequence} - Removed ${x.count} item/s (id:${x.id})"
	}
}