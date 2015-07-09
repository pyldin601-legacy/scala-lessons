package org.gemini.scala.fun

/**
 * Created by Roman on 08.07.2015
 */

object Helper {
  def isNumber(exp: String): Boolean = exp.forall(c => c.isDigit || c == '.')
  def isWord(exp: String): Boolean = exp.forall(_.isLetter)
  def fact(num: Double): Double = {
    def _fact(arg: Double, acc: Double = 1.0): Double = {
      if (arg == 1 || arg == 0) acc
      else if (arg < 0) throw new IllegalArgumentException
      else _fact(arg - 1, acc * arg)
    }
    _fact(num)
  }
}

object Calculator extends App {

  abstract class Token
  abstract class Operation(val priority: Int) extends Token

  case class BinaryOperator(override val priority: Int, calc: (Double, Double) => Double) extends Operation(priority)
  case class UnaryOperator(override val priority: Int, calc: Double => Double) extends Operation(priority)

  case object ClosedBracket extends Token
  case object OpenedBracket extends Token
  case class Digit(value: Double) extends Token

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
      case "!" => UnaryOperator(4, fact)
      case v => throw new IllegalArgumentException("Unknown constant - " + v)
    }
    def run(rest: String, acc: List[String] = Nil, last: Int = 0): List[String] = {
      if (rest.isEmpty) acc
      else {
        val (t, j) = tokenize(rest.head)
        if (acc.nonEmpty && t == last && j)
          run(rest.tail, acc.init :+ (acc.last + rest.head), last)
        else
          run(rest.tail, acc :+ rest.head.toString, t)
      }
    }
    run(exp).filterNot(_.forall(_.isSpaceChar)).map(inter)
  }

  private def arrange(exp: List[Token], stack: List[Token] = Nil, output: List[Token] = Nil): List[Token] = {
    if (exp.isEmpty) output ++ stack
    else exp.head match {
      case d: Digit => arrange(exp.tail, stack, output :+ d)
      case o: Operation =>
        if (stack.nonEmpty && stack.head.isInstanceOf[Operation])
          stack.head.asInstanceOf[Operation] match {
            case l if l.priority == o.priority =>
              arrange(exp.tail, o :: stack.tail, output :+ stack.head)
            case l if l.priority > o.priority =>
              arrange(exp, stack.tail, output :+ stack.head)
            case l =>
              arrange(exp.tail, l :: stack, output)
          }
        else
          arrange(exp.tail, exp.head :: stack, output)
      case o: OpenedBracket.type =>
        arrange(exp.tail, o :: stack, output)
      case c: ClosedBracket.type =>
        if (stack.isEmpty)
          throw new ArithmeticException("Unclosed bracket")
        if (stack.head == OpenedBracket)
          arrange(exp.tail, stack.tail, output)
        else
          arrange(exp, stack.tail, output :+ stack.head)
      case x =>
        throw new UnsupportedOperationException("Unknown operation - " + x)
    }
  }

  private def calc(arranged: List[Token], stack: List[Double] = Nil): Double = {
    if (arranged.isEmpty) stack.last
    else arranged.head match {
      case d: Digit =>
        calc(arranged.tail, stack :+ d.value)
      case o: BinaryOperator =>
        calc(arranged.tail, stack.dropRight(2) :+ stack.takeRight(2).reduce(o.calc))
      case f: UnaryOperator =>
        calc(arranged.tail, stack.init :+ f.calc(stack.last))
    }
  }

  def eval(expression: String): Double = calc(arrange(split(expression)))

  println(eval("(cos(pi * 2) + 1) ^ 8 / 4"))

}
