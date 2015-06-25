package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */

import scala.io.StdIn.readLine

object NoVariable extends App {
  println(readLine("Enter your name: ") match {
    case name if name.length > 0 =>
      print("Hello, " + name + ", ")
      readLine("how old are you? ").toInt match {
        case age if age < 18 => "You're so young!"
        case age if age > 50 => "You're so old!"
        case age => "Okay, you are " + age + " old!"
      }
    case _ => "Bye!"
  })
}
