package org.gemini.scala.fun

/**
 * Created by Roman on 08.07.2015
 */

object Helper {
  def isNumber(exp: String): Boolean = exp.forall(c => c.isDigit || c == '.')

  def isWord(exp: String): Boolean = exp.forall(_.isLetter)
}

object Calculator extends App {

  import Helper._

  abstract class Token
  
  case object ClosedBracket extends Token

  case object OpenedBracket extends Token

  case class Digit(value: Double) extends Token

  class Operation(val priority: Int) extends Token

  abstract class BinaryOperator(override val priority: Int, val calc: (Double, Double) => Double) extends Operation(priority)

  abstract class UnaryOperator(override val priority: Int, val calc: Double => Double) extends Operation(priority)

  case class LoOperator(override val calc: (Double, Double) => Double) extends BinaryOperator(0, calc)

  case class MiOperator(override val calc: (Double, Double) => Double) extends BinaryOperator(1, calc)

  case class HiOperator(override val calc: (Double, Double) => Double) extends BinaryOperator(2, calc)

  case class MathFunction(override val calc: Double => Double) extends UnaryOperator(3, calc)

  def split(exp: String): List[Token] = {
    val tokenize: PartialFunction[Char, (Int, Boolean)] = {
      case d if d.isDigit || d == '.' => (0, true)
      case l if l.isLetter => (1, true)
      case _ => (2, false)
    }
    val inter: PartialFunction[String, Token] = {
      case d if isNumber(d) => Digit(d.toDouble)
      case "(" => OpenedBracket
      case ")" => ClosedBracket
      case "+" => LoOperator(_ + _)
      case "-" => LoOperator(_ - _)
      case "*" => MiOperator(_ * _)
      case "/" => MiOperator(_ / _)
      case "^" => HiOperator(scala.math.pow)
      case "sin" => MathFunction(scala.math.sin)
      case "cos" => MathFunction(scala.math.cos)
      case "tan" => MathFunction(scala.math.tan)
      case "pi" => Digit(scala.math.Pi)
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

  def arrange(exp: List[Token], stack: List[Token] = Nil, output: List[Token] = Nil): List[Token] = {
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
        val fork = stack.indexOf(OpenedBracket)
        if (fork == -1)
          throw new ArithmeticException("Unclosed bracket")
        else
          arrange(exp.tail, stack.drop(fork + 1), output ++ stack.take(fork))
      case x =>
        throw new UnsupportedOperationException("Unknown operation - " + x)
    }
  }

  def calc(arranged: List[Token], stack: List[Double] = Nil): Double = {
    if (arranged.isEmpty) stack.last
    else arranged.head match {
      case d: Digit => calc(arranged.tail, stack :+ d.value)
      case o: BinaryOperator => calc(arranged.tail, stack.dropRight(2) :+ stack.takeRight(2).reduce(o.calc))
      case f: MathFunction => calc(arranged.tail, stack.init :+ f.calc(stack.last))
    }
  }

  val expression = "(cos(pi * 2) + 1) ^ 8 / 4"

  println(calc(arrange(split(expression))))

}
