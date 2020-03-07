package core.interpreter

import core.ast.Name

package object data {

  type Address = Int

  type Stack = List[Address]
  type Dump = List[Stack]
  type TiHeap = Heap[Node]
  type Globals = Map[Name, Address]

  class UnsafeMap[K,V](map: Map[K, V]) {
    def getOrThrow(key: K): V = map.getOrElse(key, throw new IllegalStateException("Unrecognized symbol: " + key))
  }

  implicit def mapToRichMap[K, V](map: Map[K, V]): UnsafeMap[K, V] = new UnsafeMap[K, V](map)

}
