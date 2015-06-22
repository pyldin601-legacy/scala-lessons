package org.gemini.scala.labs

import math.sqrt

object MutualFractions extends App {

  def isFraction(of: Int)(that: Int): Boolean = of % that == 0

  def calculateFractions(of: Int): Seq[Int] = {
    val list = (2 to sqrt(of).toInt + 1).filter(isFraction(of))
    list union list.map(of / _)
  }

  def calculateMutualFractions(value: Int*): Seq[Int] = {
    value.map(calculateFractions).reduce(_ intersect _).sorted
  }

  println(calculateMutualFractions(512, 256, 1024))

}
