package core.interpreter.data

trait Heap[T] {

  def update(address: Address, value: T): Heap[T]

  def free(address: Address): Heap[T]

  def alloc(value: T, address: Option[Address] = None): (Heap[T], Address)

  def addresses(): Set[Address]

  def reserve(): (Heap[T], Address)

  def lookup(address: Address): T
}

private case class HeapImpl[T](map: Map[Address, Option[T]]) extends Heap[T] {
  override def update(address: Address, value: T): Heap[T] = HeapImpl[T](map.updated(address, Some(value)))

  override def free(address: Address): Heap[T] = HeapImpl[T](map.removed(address))

  override def alloc(value: T, address: Option[Address] = None): (Heap[T], Address) = {
    val addressToUse = address.getOrElse(nextFreeAddress())
    if (address.isDefined && map.getOrThrow(address.get).isDefined) {
      throw new IllegalArgumentException("Address already assigned to value: " + map.getOrThrow(address.get))
    }
    (HeapImpl(map.updated(addressToUse, Some(value))), addressToUse)
  }

  override def addresses(): Set[Address] = map.keys.toSet

  override def reserve(): (Heap[T], Address) = {
    val reservedAddress = nextFreeAddress()
    (HeapImpl(map.updated(reservedAddress, None)), reservedAddress)
  }

  override def lookup(address: Address): T = map
    .getOrElse(address, throw new IllegalStateException(s"Address $address is not allocated"))
    .getOrElse(throw new IllegalStateException(s"Address $address is allocated to ∅"))

  def nextFreeAddress(): Address = addresses().maxOption.getOrElse(0) + 1
}

object Heap {
  def apply[T](): Heap[T] = HeapImpl(Map())
}
