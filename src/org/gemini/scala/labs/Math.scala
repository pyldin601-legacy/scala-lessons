package org.gemini.scala.labs

/**
 * Created by Roman on 24.06.2015.
 */
object Math  extends App {

  /* General recursion */
  def factorial1(x: Int): BigInt =
    if (x == 0) 1 else x * factorial1(x - 1)

  /* Tail recursion */
  def factorial2(x: Int): BigInt = {
    def _fact(x: Int, acc: BigInt = 1): BigInt =
      if (x == 0) acc else _fact(x - 1, x * acc)
    _fact(x)
  }

  println(factorial2(10000))

}
