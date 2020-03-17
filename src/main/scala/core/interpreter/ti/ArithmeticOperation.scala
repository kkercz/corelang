package core.interpreter.ti


sealed trait ArithmeticOperation {
  def symbol: String
  def arity: Int
  def apply(args: List[Int]): Int
}

case object ArithmeticOperation {

  case object Negation extends ArithmeticOperation {
    override def symbol: String = "negate"
    override def arity: Int = 1
    override def apply(args: List[Int]): Int = 0 - args.head
  }

  abstract class BinaryPrimitive(opSymbol: String, op: (Int, Int) => Int) extends ArithmeticOperation {
    override def symbol: String = opSymbol
    override def arity: Int = 2
    override def apply(args: List[Int]): Int = op(args.head, args.drop(1).head)
  }

  case object Addition extends BinaryPrimitive("+", _ + _)
  case object Subtraction extends BinaryPrimitive("-", _ - _)
  case object Multiplication extends BinaryPrimitive("*", _ * _)
  case object Division extends BinaryPrimitive("/", _ / _)
  case object Reminder extends BinaryPrimitive("%", _ % _)

  val all: List[ArithmeticOperation] = List(Negation, Addition, Subtraction, Multiplication, Division, Reminder)

}
