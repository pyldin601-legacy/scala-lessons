package org.gemini.scala.labs

/**
 * Created by roman on 12.07.15
 */
object Incremental extends App {

  def pairs(f: (Int, Int) => Boolean)(initial: Int): Int => Boolean = {
    var prev = initial
    (num: Int) => f(num, prev) match {
      case true => prev = num; true
      case false => false
    }
  }



  val numbers = 0 to 1000

  val filtered = numbers filter pairs(_ / _ == 2)(1)

  println(filtered)

}
