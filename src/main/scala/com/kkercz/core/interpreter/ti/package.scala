package com.kkercz.core.interpreter

import com.kkercz.core.ast.Name

package object ti {

  type Address = Int

  type Stack = List[Node]
  type Dump = List[Stack]
  type TiHeap = Heap[Node]
  type Globals = Map[Name, Address]

}
