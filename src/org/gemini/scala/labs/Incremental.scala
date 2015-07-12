package org.gemini.scala.labs

/**
 * Created by roman on 12.07.15
 */
object Incremental extends App {

  def increment: Stream[BigInt] = 1 #:: increment map (_ * 2)

  increment take 100 foreach println

}
