package org.gemini.scala.fun

import scala.collection.mutable

/**
 * Created by Roman on 24.06.2015.
 */
object Brackets extends App {

  /**
   * Validates the correctness of nested brackets
   * @param str String to validate
   * @return
   */
  def validate(str: String): Boolean = {
    val brackets = Array('(', ')', '[', ']', '{', '}')
    val filtered = str.filter(brackets contains _).toCharArray
    def _val(chars: Array[Char], current: Option[Int] = None): Boolean = {
      if (chars.isEmpty) return current.isEmpty

      val index = brackets.indexOf(chars.head)

      if ((index & 1) == 0) _val(chars.tail, Some(index))
      else if (current.isDefined && index - current.get == 1) true
      else false
    }
    _val(filtered)
  }

  println(validate("[Hello]"))
  println(validate("[Hello{}]()"))
  println(validate("Test[(])"))
  println(validate("Test Test"))
  println(validate("{{{{}}}}"))
  println(validate("{hello"))
  println(validate("hello}"))

}
