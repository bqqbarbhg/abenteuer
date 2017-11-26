package vm

object Entity {
  var serialCounter: Int = 0

  def nextSerial(): Int = {
    serialCounter += 1
    serialCounter
  }
}

class Entity(val debugName: String) extends Comparable[Entity] {
  val serial = Entity.nextSerial()

  override def toString: String = debugName
  override def compareTo(other: Entity) = serial.compareTo(other.serial)
}
