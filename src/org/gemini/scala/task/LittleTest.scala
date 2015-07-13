package org.gemini.scala.task

/**
 * Created by Roman on 13.07.2015
 */
object LittleTest extends App {

  def abs(number: Int): Int = if (number < 0) -number else number

  def sumOfNumbers(last: Int): Int = 1 to last sum

  def vasya(number: Int): Int = {
    if (number % 5 != 0)
      throw new IllegalArgumentException("Number must be multiple of 5")
    val a = (number - 5) / 10
    a * (a + 1) * 100 + 25
  }

  def split(numbers: List[Int]): (List[Int], List[Int]) =
    numbers partition(_ % 2 != 0)

  def move(mv: String): String = {
    val letters = List("A", "B", "C", "D", "E", "F", "G", "H")
    try {
      val firstX: Int = letters.indexOf(mv.substring(0, 1))
      val firstY: Int = mv.substring(1, 2).toInt
      val secondX: Int = letters.indexOf(mv.substring(3, 4))
      val secondY: Int = mv.substring(4, 5).toInt
      if (abs(firstX - secondX - firstY - secondY) == 1) "YES"
      else "NO"
    } catch {
      case e: NumberFormatException => "ERROR"
    }
  }

  println(List(987531, 234, 86364) reduce scala.math.max)

}
