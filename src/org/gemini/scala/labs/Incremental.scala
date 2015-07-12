package org.gemini.scala.labs

/**
 * Created by roman on 12.07.15
 */
object Incremental extends App {

  def str: Stream[BigInt] = 1 #:: str map (_ * 2)

  str take 100 foreach println

}
