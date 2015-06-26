package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015.
 */
object Composition extends App {
  val a: Int => Int = _ + 10
  val b: Int => Int = _ * 5
  val c: Int => Int = _ / 2
  val comp = a compose b compose c
  println(comp(100))
}
