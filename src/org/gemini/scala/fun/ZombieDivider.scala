package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */
object ZombieDivider extends App {
  class Zombie() {
    def /(q: Int): List[Zombie] = {
      List.fill(q)(new Zombie)
    }
    override def toString: String = "Zombie!"
  }

  val zombie = new Zombie()

  println(zombie)


}

