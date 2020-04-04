package core.interpreter.ti

import core.interpreter.ti.data.{Node, State}
import core.lang.Prelude


sealed trait BuiltInFunction {
  def symbol: String
  def arity: Int
  def evaluatedArgumentsExpected: Int = arity
  def apply(state: State, args: List[Node]): (State, Option[Node])
}

case object BuiltInFunction {

  val True: Node.Constr = Node.Constr(Prelude.trueLiteral)
  val False: Node.Constr = Node.Constr(Prelude.falseLiteral)

  abstract class PureBuiltInFunction extends BuiltInFunction {
    final override def apply(state: State, args: List[Node]): (State, Option[Node]) = (state, Some(apply(args)))
    def apply(args: List[Node]): Node
  }

  case object Negation extends PureBuiltInFunction {
    override def symbol: String = "negate"
    override def arity: Int = 1
    override def apply(args: List[Node]): Node = withUnsafeCasts(symbol, Node.Num(0 - args.head.asInstanceOf[Node.Num].value))
  }

  case object Abort extends PureBuiltInFunction {
    override def symbol: String = "abort"
    override def arity: Int = 0
    override def apply(args: List[Node]): Node = throw new IllegalStateException("The program is faulty and has been aborted")
  }

  case object Print extends BuiltInFunction {
    override def symbol: String = "print"
    override def arity: Int = 2
    override def evaluatedArgumentsExpected: Int = 1
    override def apply(state: State, args: List[Node]): (State, Option[Node]) = (state.appendToOutput(args.head), Some(args.tail.head))
  }

  case object Stop extends BuiltInFunction {
    override def symbol: String = "stop"
    override def arity: Int = 0
    override def apply(state: State, args: List[Node]): (State, Option[Node]) = (state, None)
  }

  case object Not extends PureBuiltInFunction {
    override def symbol: String = "not"
    override def arity: Int = 1
    override def apply(args: List[Node]): Node = withUnsafeCasts(symbol, if (args.head.asInstanceOf[Node.Constr] == True) False else True)
  }

  case class BinaryFunction[T <: Node](opSymbol: String, op: (T, T) => Node) extends PureBuiltInFunction {
    override def symbol: String = opSymbol
    override def arity: Int = 2
    override def apply(args: List[Node]): Node = withUnsafeCasts(opSymbol, op(args.head.asInstanceOf[T], args.drop(1).head.asInstanceOf[T]))
  }

  def arithmeticOperation(symbol: String, op: (Int, Int) => Int): BinaryFunction[Node.Num] =
    BinaryFunction[Node.Num](symbol, (n1, n2) => Node.Num(op.apply(n1.value, n2.value)))

  def arithmeticComparison(symbol: String, op: (Int, Int) => Boolean): BinaryFunction[Node.Num] = BinaryFunction[Node.Num](
    symbol, (n1, n2) => if (op.apply(n1.value, n2.value)) True else False)

  def withUnsafeCasts[T](name: String, op: => T): T =
    try { op } catch {
      case _: ClassCastException => throw new IllegalArgumentException(s"Incorrect type of arguments provided to function: $name")
      case ex: Throwable => throw ex
    }

  val arithmeticOperations = List(
    Negation,
    arithmeticOperation("+", _ + _),
    arithmeticOperation("-", _ - _),
    arithmeticOperation("*", _ * _),
    arithmeticOperation("/", _ / _),
    arithmeticOperation("%", _ % _))

  val arithmeticComparisons = List(
    arithmeticComparison("<", _ < _),
    arithmeticComparison("<=", _ <= _),
    arithmeticComparison(">", _ > _),
    arithmeticComparison(">=", _ >= _),
    arithmeticComparison("==", _ == _),
    arithmeticComparison("!=", _ != _))

  val booleanOperators = List(
    Not,
    BinaryFunction[Node.Constr]("&&", (op1, op2) => if (op1 == True) op2 else False),
    BinaryFunction[Node.Constr]("||", (op1, op2) => if (op1 == True) True else op2)
  )

  val specialPrimitives = List(Abort, Print, Stop)

  val all: List[BuiltInFunction] = specialPrimitives ++ arithmeticOperations ++ arithmeticComparisons ++ booleanOperators
}
