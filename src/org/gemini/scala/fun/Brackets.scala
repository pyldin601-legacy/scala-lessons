package org.gemini.scala.fun

/**
 * Created by Roman on 24.06.2015.
 */
object Brackets extends App {

  /**
   * Validates the correctness of nested brackets and quotes
   * @param str String with text to validate
   * @return Boolean
   */
  def validateBrackets(str: String): Boolean = {
    val brackets = Array('(', ')', '[', ']', '{', '}')
    val filtered = str.filter(brackets contains _).toCharArray
    def _val(chars: Array[Char], current: Option[Int] = None): Boolean = {
      if (chars.isEmpty) return current.isEmpty

      val index = brackets.indexOf(chars.head)

      if (current.isDefined && index == current.get) true
      else if ((index & 1) == 0) _val(chars.tail, Some(index))
      else if (current.isDefined && index - current.get == 1) true
      else false
    }
    _val(filtered)
  }

  val testLines = Array(
    "[Hello]",
    "[Hello{}]()",
    "Test this [(])",
    "This may { fail Test",
    "({{{[{}]}}})",
    "{hello",
    "hello}"
  )

  testLines.map((t) => t + " -> " + validateBrackets(t)).foreach(println)

}
