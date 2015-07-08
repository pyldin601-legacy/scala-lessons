package org.gemini.scala.labs

/**
 * Created by Roman on 08.07.2015
 */
object Evaluate extends App {

  val expression = "12 + 7"

  def isOperator(op: String) = op match {
    case "+" | "-" | "*" | "/" => true
    case _ => false
  }

  def isDigit(that: String): Boolean = that.forall(c => c.isDigit || c == '.')

  def priority(op: String) = op match {
    case "*" | "/" => 2
    case "+" | "-" => 3
  }

  def op(op: String)(x: Double, y: Double) = op match {
    case "+" => x + y
    case "-" => x - y
    case "*" => x * y
    case "/" => x / y
  }

  def parse(expression: String, output: List[String] = Nil, stack: List[String] = Nil): List[String] = {
    if (expression.isEmpty) output ++ stack.reverseMap(_.toString)
    else if (expression.head == ' ')
      parse(expression.tail, output, stack)
    else
      if (expression.head.isDigit | expression.head == '.')
//        if (output.nonEmpty && isDigit(output.last))
//          parse(expression.tail, output.init :+ (output.last + expression.head), stack)
//        else
          parse(expression.tail, output :+ expression.head.toString, stack)
      else if (isOperator(expression.head.toString))
        if (stack.nonEmpty && priority(expression.head.toString) < priority(stack.last.toString))
          parse(expression.tail, output, stack :+ expression.head.toString)
        else
          parse(expression.tail, output ++ stack.reverseMap(_.toString), List(expression.head.toString))
      else throw new ArithmeticException
  }

  def eval(expression: String): Double = {
    val queue = parse(expression)
    println(queue)
    def _evaluate(rest: List[String], acc: Double, stack: List[Double]): Double = {
      if (rest.isEmpty) acc
      else if (isOperator(rest.head)) {
        val r = stack.takeRight(2).reduceLeft(op(rest.head))
        _evaluate(rest.tail, r, stack.dropRight(2) :+ r)
      } else
        _evaluate(rest.tail, acc, stack :+ rest.head.toDouble)
    }
    _evaluate(queue, 0, Nil)
  }

  println(eval(expression))

}
