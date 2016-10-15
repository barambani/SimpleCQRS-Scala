package SimpleCqrsScala.CommandSide

import SimpleCqrsScala.CommandSide.Domain._

trait Show[A] {
	def stringFor(x: A): String
}

object Show {
	implicit lazy val InventoryItemCreatedShow = new Show[InventoryItemCreated] {
		def stringFor(x: InventoryItemCreated): String = s"seq: ${x.sequence} - Item ${x.name} created (id:${x.id})"
	}
	implicit lazy val InventoryItemDeactivatedShow = new Show[InventoryItemDeactivated] {
		def stringFor(x: InventoryItemDeactivated): String = s"seq: ${x.sequence} - Item deactivated (id:${x.id})"
	}
	implicit lazy val InventoryItemRenamedShow = new Show[InventoryItemRenamed] {
		def stringFor(x: InventoryItemRenamed): String = s"seq: ${x.sequence} - Item renamed to ${x.newName} (id:${x.id})"
	}
	implicit lazy val ItemsCheckedInToInventoryShow = new Show[ItemsCheckedInToInventory] {
		def stringFor(x: ItemsCheckedInToInventory): String = s"seq: ${x.sequence} - Check-in of ${x.count} item/s (id:${x.id})"
	}
	implicit lazy val ItemsRemovedFromInventoryShow = new Show[ItemsRemovedFromInventory] {
		def stringFor(x: ItemsRemovedFromInventory): String = s"seq: ${x.sequence} - Removed ${x.count} item/s (id:${x.id})"
	}
}