package org.gemini.scala.labs

/**
 * Created by Roman on 08.07.2015
 */
object Evaluate extends App {

  val expression = "10 + 15 * 2"

  abstract class Token

  case class Operand(value: String) extends Token {
    def add(other: Char): Operand = Operand(value + other)
    def calc: Double = value.toDouble
    override def toString: String = value
  }

  case class Operator(value: Char) extends Token {
    def eval(op: List[Double]): Double = {
      value match {
        case '+' => op.sum
        case '-' => op.reduceLeft(_ - _)
        case '/' => op.reduceLeft(_ / _)
        case '*' => op.product
      }
    }
    def priority: Int = value match {
      case '*' | '/' => 1
      case '+' | '-' => 2
    }
    override def toString: String = value.toString
  }


  def parse(exp: String, acc: List[Token] = Nil, stack: List[Operator] = Nil): List[Token] = {
    if (exp.isEmpty) acc ++ stack.reverse
    else if (exp.head == ' ')
      parse(exp.tail, acc, stack)
    else if (exp.head.isDigit || exp.head == '.')
      if (acc.nonEmpty && acc.last.isInstanceOf[Operand])
        parse(exp.tail, acc.init :+ acc.last.asInstanceOf[Operand].add(exp.head), stack)
      else
        parse(exp.tail, acc :+ Operand(exp.head.toString), stack)
    else
      if (stack.nonEmpty && Operator(exp.head).priority > stack.last.priority)
        parse(exp.tail, acc ++ stack.reverse, List(Operator(exp.head)))
      else
        parse(exp.tail, acc :+ Operand(""), stack :+ Operator(exp.head))

  }

  def eval(tokens: List[Token], stack: List[Double] = Nil): Double = {
    if (tokens.isEmpty) stack.last
    else tokens.head match {
      case operand: Operand =>
        eval(tokens.tail, stack :+ operand.calc)
      case operator: Operator =>
        eval(tokens.tail, stack.dropRight(2) :+ operator.eval(stack.takeRight(2)))
    }

  }

  println(eval(parse(expression)))

}
