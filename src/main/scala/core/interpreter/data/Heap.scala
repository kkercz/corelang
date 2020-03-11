package core.interpreter.data

case class Heap[T](map: Map[Address, Option[T]]) {

  def update(address: Address, value: T): Heap[T] = Heap[T](map.updated(address, Some(value)))

  def free(address: Address): Heap[T] = Heap[T](map.removed(address))

  def alloc(value: T, address: Option[Address] = None): (Heap[T], Address) = {
    val addressToUse = address.getOrElse(nextFreeAddress())
    if (address.isDefined && map.getOrThrow(address.get).isDefined) {
      throw new IllegalArgumentException("Address already assigned to value: " + map.getOrThrow(address.get))
    }
    (Heap(map.updated(addressToUse, Some(value))), addressToUse)
  }

  def addresses(): Set[Address] = map.keys.toSet

  def reserve(): (Heap[T], Address) = {
    val reservedAddress = nextFreeAddress()
    (Heap(map.updated(reservedAddress, None)), reservedAddress)
  }

  def lookup(address: Address): T = map
    .getOrElse(address, throw new IllegalStateException(s"Address $address is not allocated"))
    .getOrElse(throw new IllegalStateException(s"Address $address is allocated to ∅"))

  private[this] def nextFreeAddress(): Address = addresses().maxOption.getOrElse(0) + 1
}


object Heap {
  def empty[T](): Heap[T] = Heap(Map())
}
