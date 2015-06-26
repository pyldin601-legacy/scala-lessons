package org.gemini.scala.labs



object Extending extends App {

  class Wrapper[T](val t: T) extends T {

  }

  class Inner {
    def hello(): Unit = println("Hello")
  }

  val wrapped = new Wrapper[Inner](new Inner)

  println(wrapped)


}
