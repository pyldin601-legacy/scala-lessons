package org.gemini.scala.labs

/**
 * Created by roman on 27.06.15
 */
object Fibonacci extends App {

  val fib: Stream[BigInt] =
    0 #:: 1 #:: (fib zip fib.tail).map(x => x._1 + x._2)

  def fact(n: BigInt): BigInt = BigInt(1) to n product

  println(fact(100))

}

