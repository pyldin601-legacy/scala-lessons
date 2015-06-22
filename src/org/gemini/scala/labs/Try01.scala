package org.gemini.scala.labs


import scala.math._


object Try01 extends App {

  def fact(n: Int): BigInt =
    n match {
      case 0 => 1
      case _ => n * fact(n - 1)
    }

  println(for (x <- 0 until 10) fact(x))

  (0 until 10) map fact foreach println

}
