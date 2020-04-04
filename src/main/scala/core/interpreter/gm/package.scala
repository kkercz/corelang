package core.interpreter

import core.interpreter.data.Heap
import core.interpreter.gm.data.{Instruction, Node}

package object gm {
  type GmHeap = Heap[Node]
  type GmCode = List[Instruction]
}
