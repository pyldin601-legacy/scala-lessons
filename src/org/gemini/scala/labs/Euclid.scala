package org.gemini.scala.labs

/**
 * Created by Roman on 13.07.2015
 */
object Euclid extends App {

  def euclid(a: Int, b: Int): Int =
    if (b == 0) a else euclid(b, a % b)

  def euclid(numbers: Int*): Int =
    numbers reduce euclid

  println(euclid(1024, 512))

}
