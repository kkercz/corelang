package core.interpreter.ti.gc

import core.interpreter.data.{Address, Heap}
import core.interpreter.ti.TiHeap
import core.interpreter.ti.data.Node.Alternative
import core.interpreter.ti.data.{Node, TiState}
import core.util.Time

import scala.annotation.tailrec

case object GarbageCollector {

  def gc(state: TiState): TiState = {
    if (state.heap.addresses().size < 1000) {
      state
    } else {
      val initialAddresses = state.stack ++ state.dump.flatten ++ state.globals.values
      val ((fromToMapping, toSpace), elapsedTime) = Time.measure(twoSpaceGc(initialAddresses, state.heap))

      val reclaimedSpace = state.heap.addresses().size - toSpace.addresses().size

      state
        .withHeap(toSpace)
        .withStats(state.stats.updateGcStats(reclaimedSpace, elapsedTime))
        .withStack(state.stack.map(a => fromToMapping(a)))
        .withGlobals(state.globals.map({ case (key, value) => (key, fromToMapping(value)) }))
        .withDump(state.dump.map(stack => stack.map(a => fromToMapping(a))))
    }
  }


  def twoSpaceGc(initialAddresses: List[Address], fromSpace: TiHeap): (Map[Address, Address], TiHeap) = {
    @tailrec
    def twoSpaceGcLoop(fromToMapping: Map[Address, Address], toScavenge: List[Address], toSpace: TiHeap): (Map[Address, Address], TiHeap) = {
      if (toScavenge.isEmpty) {
        (fromToMapping, toSpace)
      } else {
        val (newMapping, toScavenge2, newToSpace) = scavenge(fromToMapping, toScavenge, fromSpace, toSpace)
        twoSpaceGcLoop(newMapping, toScavenge2, newToSpace)
      }
    }

    val (initialMapping, initialEvacuated, toSpace) = evacuate(Map(), initialAddresses, fromSpace, Heap.empty())

    twoSpaceGcLoop(initialMapping, initialEvacuated, toSpace)
  }

  def scavenge(mapping: Map[Address, Address], addresses: Iterable[Address], fromSpace: TiHeap, toSpace: TiHeap): (Map[Address, Address], List[Address], TiHeap) = {
    addresses.foldLeft((mapping, List[Address](), toSpace))({ case ((mapping, toScavenge, toSpace), addr) => toSpace.lookup(addr) match {
      case Node.App(a1, a2) =>
        val (newMapping, evacuated, toSpace2) = evacuate(mapping, List(a1, a2), fromSpace, toSpace)
        val (a1Mapped, a2Mapped) = (newMapping(a1), newMapping(a2))
        (newMapping, evacuated ++ toScavenge, toSpace2.update(addr, Node.App(a1Mapped, a2Mapped)))
      case Node.Constr(expr, args) =>
        val (newMapping, evacuated, toSpace2) = evacuate(mapping, args, fromSpace, toSpace)
        (newMapping, evacuated ++ toScavenge, toSpace2.update(addr, Node.Constr(expr, args.map(a => newMapping(a)))))
      case Node.Case(expr, alternatives) =>
        val (newMapping, evacuated, toSpace2) = evacuate(mapping, expr :: alternatives.flatMap(a => a.env.values), fromSpace, toSpace)
        val newNode = Node.Case(
          newMapping(expr),
          alternatives.map(a => Alternative(a.expr, a.env.map({ case (key, value) => (key, newMapping(value)) })))
        )
        (newMapping, evacuated ++ toScavenge, toSpace2.update(addr, newNode))
      case Node.Ref(_) => throw new IllegalStateException("Ref node should not have been evacuated")
      case _ => (mapping, toScavenge, toSpace)
    }})
  }

  def evacuate(mapping: Map[Address, Address], addresses: List[Address], fromSpace: TiHeap, toSpace: TiHeap): (Map[Address, Address], List[Address], TiHeap) =
    addresses.foldLeft((mapping, List[Address](), toSpace))({ case ((mapping, evacuated, heap), addr) =>
      if (mapping.contains(addr)) {
        (mapping, evacuated, heap)
      } else {
        val (fromAddresses, node) = deref(fromSpace, addr)
        val (newHeap, newAddress) = heap.alloc(node)
        val newMapping = fromAddresses.foldLeft(mapping)((m, a) => m.updated(a, newAddress))
        (newMapping, newAddress :: evacuated, newHeap)
      }
    })

  def deref(heap: TiHeap, address: Address): (List[Address], Node) =
    heap.lookup(address) match {
      case Node.Ref(a) =>
        val (addresses, node) = deref(heap, a)
        (address :: addresses, node)
      case node => (List(address), node)
    }

}
