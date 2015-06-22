package org.gemini.scala.labs


import scala.math._


object Try01 extends App {

  def fact(n: Int, acc: BigInt = 1): BigInt =
    if (n <= 1) acc
    else fact(n - 1, acc * n)

  (0 until 10) map(fact(_)) foreach println

}
