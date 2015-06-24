package org.gemini.scala.labs

/**
 * Created by Roman on 24.06.2015.
 */
object For extends App {

  val result = for {
    i <- 0 until 5 filter(_ % 2 == 0)
    j <- 0 until 5 filter(_ % 2 == 1)
  } yield (i, j)

  println(result)

}
