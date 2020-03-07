package com.kkercz.core.interpreter.data

trait Heap[T] {

  def update(address: Address, value: T): Heap[T]

  def free(address: Address): Heap[T]

  def alloc(value: T): (Heap[T], Address)

  def addresses(): Set[Address]

  def lookup(address: Address): Option[T]
}

private case class HeapImpl[T](map: Map[Address, T]) extends Heap[T] {
  override def update(address: Address, value: T): Heap[T] = HeapImpl[T](map.updated(address, value))

  override def free(address: Address): Heap[T] = HeapImpl[T](map.removed(address))

  override def alloc(value: T): (Heap[T], Address) = {
    val nextFreeAddress = addresses().maxOption.getOrElse(0) + 1
    (HeapImpl(map.updated(nextFreeAddress, value)), nextFreeAddress)
  }

  override def addresses(): Set[Address] = map.keys.toSet

  override def lookup(address: Address): Option[T] = map.get(address)
}

object Heap {
  def apply[T](): Heap[T] = HeapImpl(Map())
}
