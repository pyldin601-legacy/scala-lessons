package org.gemini.scala.labs

/**
 * Created by Roman on 26.06.2015.
 */
object Ranges extends App {
  for (i <- 0 to 10) {
    println(i)
  }
  for (i <- 0 until 10) {
    println(i)
  }
  for (i <- 0 until 10 by 3) {
    println(i)
  }
}
