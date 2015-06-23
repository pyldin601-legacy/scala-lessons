package org.gemini.scala.labs

import math.sqrt

object MutualFractions extends App {

  def isPrime(number: Int): Boolean =
    number != 1 && 2.until(sqrt(number).toInt + 1).forall(number % _ != 0)

  def calculatePrimes(number: Int): Seq[Int] = {
    2 +: (3 until number by 2).filter(isPrime)
  }

  def calculateFractions(number: Int): Seq[Int] = {
    def isFraction(x: Int) = number % x == 0
    val list = (2 to sqrt(number).toInt + 1).filter(isFraction)
    list union list.reverseMap(number / _)
  }

  def calculateMutualFractions(value: Int*): Seq[Int] = {
    value.map(calculateFractions).reduce(_ intersect _)
  }
  
  println("Hello, World!")

//  var counter = 0
//  val start = System.currentTimeMillis()
//
//  while (System.currentTimeMillis() - start < 5000) {
//    calculateMutualFractions(1024, 2048, 512, 323767, 128)
//    counter += 1
//  }



}
