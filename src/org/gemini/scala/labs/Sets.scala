package org.gemini.scala.labs


/**
 * Created by Roman on 26.06.2015
 */

object Lib {
  type IntSet = Int => Boolean
  implicit class Wrapper[A](p0: A => Boolean) {
    def &(p1: A => Boolean): A => Boolean =
      x => p0(x) & p1(x)
    def |(p1: A => Boolean): A => Boolean =
      x => p0(x) | p1(x)
    def ?(elem: A): Boolean =
      p0(elem)
  }
}

object Sets extends App {
  import Lib._

  val even: IntSet = _ % 2 == 0
  val set1: IntSet = _ > 10
  val set2: IntSet = _ < 20
  val subset = (set1 & set2) & even

  val items = 0 to 1000 filter subset

  println(items)

}
