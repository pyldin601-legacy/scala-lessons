package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */
object ZombieDivider extends App {
  case class Zombie() {
    def /(q: Int): List[Zombie] = {
      List.fill(q)(this)
    }
  }

  val zombie = Zombie()

  println(zombie)

  println(zombie / 10)

}

