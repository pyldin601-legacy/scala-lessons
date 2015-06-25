package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */

import scala.io.StdIn.readLine

object NoVariable extends App {
  println(try {
    readLine("How old are you? ").toInt match {
      case age if age < 18 => "You're so young!"
      case age if age > 50 => "You're so old!"
      case age => "Okay, you are " + age + " old!"
    }
  } catch {
    case e: NumberFormatException =>
      println("You're entered incorrect value.")
      sys.exit()
  })
}
