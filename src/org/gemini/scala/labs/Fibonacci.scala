package org.gemini.scala.labs

/**
 * Created by roman on 27.06.15
 */
object Fibonacci extends App {

  implicit class IntFactorial(val value: Int) extends AnyVal {
    def ! : BigInt = BigInt(1) to BigInt(value) product
  }

  val fib: Stream[BigInt] =
    0 #:: 1 #:: (fib zip fib.tail).map(x => x._1 + x._2)

  def fact(n: BigInt): BigInt = BigInt(1) to n product



  println(10!)

}

