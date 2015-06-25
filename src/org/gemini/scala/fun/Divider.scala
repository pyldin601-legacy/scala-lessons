package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */
object Divider extends App {
  case class SuperClass() {
    def /(q: Int): List[SuperClass] = {
      List.fill(q)(this)
    }
  }

  val item = SuperClass()

  println(item)

  println(item / 10)

}
