package core.interpreter

import core.interpreter.data.Heap
import core.interpreter.ti.data.Node

package object ti {
  type TiHeap = Heap[Node]
}
