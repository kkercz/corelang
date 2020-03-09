package core.interpreter

import core.ast.Name

package object data {

  type Address = Int

  type Stack = List[Address]
  type Dump = List[Stack]
  type TiHeap = Heap[Node]
  type Globals = Map[Name, Address]

  class InterpreterMap[K,V](map: Map[K, V]) {
    def getOrThrow(key: K): V = map.getOrElse(key, throw new IllegalStateException("Unrecognized symbol: " + key))
  }

  implicit def toInterpreterMap[K, V](map: Map[K, V]): InterpreterMap[K, V] = new InterpreterMap[K, V](map)

}
