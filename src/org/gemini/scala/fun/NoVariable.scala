package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */

import scala.math._

object NoVariable extends App {

  implicit class ForwardPipe[A](val value: A) extends AnyVal {
    def =>>[B](f: A => B): B = f(value)
  }

  System.currentTimeMillis() match {
    case o if o % 2 == 1 =>
      o / 10 =>> (10 / _) =>> (_ * 5) =>> (_.asInstanceOf[Double]) =>> sin =>> println
    case e =>
      "EVEN" =>> println
  }

}
