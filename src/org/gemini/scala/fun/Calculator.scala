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

  class Token
  class Operator(val priority: Int) extends Token

  case object ClosedBracket extends Operator(10)
  case object OpenedBracket extends Operator(10)

  case class UnOperator(name: String, override val priority: Int, calc: Double => Double) extends Operator(priority)
  case class BiOperator(name: String, override val priority: Int, calc: (Double, Double) => Double) extends Operator(priority)
  case class Digit(value: Double) extends Token { def get = value.toDouble }

  val expression = "(sin (1.1 + 10) / 2) + 10"

  val tokenize: PartialFunction[Char, (Int, Boolean)] = {
    case d if d.isDigit || d == '.'   => (0, true)
    case l if l.isLetter              => (1, true)
    case _                            => (2, false)
  }

  val inter: PartialFunction[String, Token] = {
    case d if isNumber(d) => Digit(d.toDouble)
    case "("              => OpenedBracket
    case ")"              => ClosedBracket
    case "+"              => BiOperator("+", 1, _ + _)
    case "-"              => BiOperator("-", 1, _ - _)
    case "*"              => BiOperator("*", 2, _ * _)
    case "/"              => BiOperator("/", 2, _ / _)
    case "sin"            => UnOperator("sin", 3, scala.math.sin)
    case "cos"            => UnOperator("cos", 3, scala.math.cos)
  }

  def split(exp: String, f: Char => (Int, Boolean)): List[String] = {
    def run(rest: String, acc: List[String] = Nil, last: Int = 0): List[String] = {
      if (rest.isEmpty) acc
      else {
        val (t, j) = f(rest.head)
        if (acc.nonEmpty && t == last && j)
          run(rest.tail, acc.init :+ (acc.last + rest.head), last)
        else
          run(rest.tail, acc :+ rest.head.toString, t)
      }
    }
    run(exp.filterNot(_.isSpaceChar))
  }

  def eval(exp: String): List[Token] = {
    split(expression, tokenize).collect(inter)
  }

  println(eval(expression))

}
