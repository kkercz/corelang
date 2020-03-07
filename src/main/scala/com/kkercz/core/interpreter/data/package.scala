package com.kkercz.core.interpreter

import com.kkercz.core.ast.Name

package object data {

  type Address = Int

  type Stack = List[Address]
  type Dump = List[Stack]
  type TiHeap = Heap[Node]
  type Globals = Map[Name, Address]

}
