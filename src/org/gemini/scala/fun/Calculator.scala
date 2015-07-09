package org.gemini.scala.fun

/**
 * Created by Roman on 08.07.2015
 */

object Helper {
  def isNumber(exp: String): Boolean = exp.forall(c => c.isDigit || c == '.')
  def isWord(exp: String): Boolean = exp.forall(_.isLetter)
}

object Calculator extends App {

  abstract class Token
  abstract class Operation(val priority: Int) extends Token

  case class BinaryOperator(override val priority: Int, calc: (Double, Double) => Double) extends Operation(priority)
  case class UnaryOperator(override val priority: Int, calc: Double => Double) extends Operation(priority)

  case object ClosedBracket extends Token
  case object OpenedBracket extends Token
  case class Digit(value: Double) extends Token

  case class Output(values: List[Double] = Nil) {
    def add(digit: Digit) = Output(values :+ digit.value)
    def apply(op: Operation) =
      op match {
        case b: BinaryOperator =>
          if (values.length < 2) throw new ArithmeticException("Not enough of operands")
          else Output((values dropRight 2) :+ ((values takeRight 2) reduce b.calc))
        case u: UnaryOperator =>
          if (values.isEmpty) throw new ArithmeticException("Not enough of operands")
          else Output(values.init :+ u.calc(values.last))
        case _ =>
          throw new UnsupportedOperationException
      }
    def result =
      if (values.length != 1) throw new ArithmeticException("Syntax error")
      else values.head
  }

  case class Container(output: Output = new Output(), stack: List[Token] = Nil) {
    def stackIsEmpty = stack.isEmpty
    def lastIsOperation = stack.head.isInstanceOf[Operation]
    def lastOperation = stack.head.asInstanceOf[Operation]
    def result: Double =
      if (stack.forall(_.isInstanceOf[Operation]))
        stack.map(_.asInstanceOf[Operation]).foldLeft(output)(_ apply _).result
      else
        throw new ArithmeticException("Closed bracket has no pair")
  }

  import Helper._

  private def split(exp: String): List[Token] = {
    val tokenize: PartialFunction[Char, (Int, Boolean)] = {
      case d if d.isDigit || d == '.' => (0, true)
      case l if l.isLetter => (1, true)
      case _ => (2, false)
    }
    val inter: PartialFunction[String, Token] = {
      case d if isNumber(d) => Digit(d.toDouble)
      case "pi" => Digit(scala.math.Pi)
      case "(" => OpenedBracket
      case ")" => ClosedBracket
      case "+" => BinaryOperator(0, _ + _)
      case "-" => BinaryOperator(0, _ - _)
      case "*" => BinaryOperator(1, _ * _)
      case "/" => BinaryOperator(1, _ / _)
      case "^" => BinaryOperator(2, scala.math.pow)
      case "sin" => UnaryOperator(3, scala.math.sin)
      case "cos" => UnaryOperator(3, scala.math.cos)
      case "tan" => UnaryOperator(3, scala.math.tan)
      case "sqrt" => UnaryOperator(3, scala.math.sqrt)
      case v => throw new IllegalArgumentException("Unknown constant - " + v)
    }
    def _split(rest: String, acc: List[String] = Nil, last: Int = 0): List[String] = {
      if (rest.isEmpty) acc
      else {
        val (t, j) = tokenize(rest.head)
        if (acc.nonEmpty && t == last && j)
          _split(rest.tail, acc.init :+ (acc.last + rest.head), last)
        else
          _split(rest.tail, acc :+ rest.head.toString, t)
      }
    }
    _split(exp).filterNot(_.forall(_.isSpaceChar)).map(inter)
  }

  private def reduce(container: Container, token: Token): Container = {
    token match {
      case d: Digit => container.copy(output = container.output add d)
      case o: Operation =>
        if (container.stack.nonEmpty && container.lastIsOperation)
          container.lastOperation match {
            case l if l.priority > o.priority => reduce(
              container.copy(container.output.apply(l), container.stack.tail), token)
            case l =>
              container.copy(stack = l :: container.stack)
          }
        else
          container.copy(stack = o :: container.stack)
      case o: OpenedBracket.type =>
        container.copy(stack = o :: container.stack)
      case c: ClosedBracket.type =>
        if (container.stackIsEmpty)
          throw new ArithmeticException("Closed bracket has no pair")
        if (container.stack.head == OpenedBracket)
          container.copy(stack = container.stack.tail)
        else
          reduce(Container(container.output.apply(container.lastOperation),
            container.stack.tail), token)
      case x =>
        throw new UnsupportedOperationException("Unknown operation - " + x)
    }
  }

  def eval(expression: String): String =
    expression + " = " + split(expression).foldLeft(Container())(reduce).result

  println(eval("sqrt((cos(pi * 2) + 1) ^ 8 / 4)"))

}
