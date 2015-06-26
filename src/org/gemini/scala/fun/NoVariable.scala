package org.gemini.scala.fun

/**
 * Created by Roman on 25.06.2015
 */

object NoVariable extends App {

  implicit class ForwardPipe[A](val value: A) extends AnyVal {
    def =>>[B](f: A => B): B = f(value)
  }

}
