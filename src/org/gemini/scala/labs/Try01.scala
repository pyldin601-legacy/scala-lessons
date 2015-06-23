package org.gemini.scala.labs


import scala.beans.BeanProperty
import scala.math._


object Try01 extends App {

  case class Person(@BeanProperty var name: String, @BeanProperty var age: Int)

  val p = new Person(name = "Roman", age = 30)

  p.setAge(p.getAge + 1)

  print(p)

}
