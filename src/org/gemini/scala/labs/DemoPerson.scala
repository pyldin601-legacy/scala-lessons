package org.gemini.scala.labs

/**
 * Created by roman on 25.06.15
 */
object DemoPerson extends App {
  case class Person(name: String, age: Int)
  val p = Person("Roman", 30)
  val Person(n, a) = p
  print(n)
}
