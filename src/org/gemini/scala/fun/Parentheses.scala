package org.gemini.scala.fun

import scala.collection.mutable

/**
 * Created by Roman on 24.06.2015.
 */
object Parentheses extends App {

  def validator(str: String): Boolean = {
    val parentheses = Array('(', ')', '[', ']', '{', '}')
    val filtered = str.filter(parentheses contains _).toCharArray
    def _val(chars: Array[Char], current: Option[Int] = None): Boolean = {
      if (chars.isEmpty) return true

      val index = parentheses.indexOf(chars.head)

      if ((index & 1) == 0) _val(chars.tail, Some(index))
      else if (current.isDefined && index - current.get == 1) true
      else false
    }
    _val(filtered)
  }

  println(validator("[Hello]"))
  println(validator("['Hello'{}]()"))
  println(validator("Test[(])"))
  println(validator("Test Test"))
  println(validator("{{{{}}}}"))

}
