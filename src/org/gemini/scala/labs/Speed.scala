package org.gemini.scala.labs

/**
 * Created by Roman on 10.07.2015
 */
object Speed extends App {
  val start = System.currentTimeMillis()
  val result = (1 to Int.MaxValue) reduce (_ - _)
  val end = System.currentTimeMillis()
  println(end - start, result)
}
