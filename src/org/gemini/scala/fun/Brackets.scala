package org.gemini.scala.fun

/**
 * Created by Roman on 24.06.2015
 * Going to learn Scala until autumn
 */
object Brackets extends App {

  /**
   * Validates the correctness of nested brackets and quotes
   * @param str String with text to validate
   * @return Boolean
   */
  def validateBrackets(str: String): Boolean = {
    val brackets = Array('(', ')', '[', ']', '{', '}')
    def _val(chars: Array[Char], current: Option[Int]): Boolean = {
      chars.isEmpty match {
        case true => current.isEmpty
        case _ =>
          brackets.indexOf(chars.head) match {
            case -1 => _val(chars.tail, current)
            case index =>
              if ((index & 1) == 0) _val(chars.tail, Some(index))
              else if (current.isDefined && index - current.get == 1) true
              else false
          }

      }

    }
    _val(str.toCharArray, None)
  }

  def validateBracketsQuotes(str: String): Boolean = {
    val brackets = Array('(', ')', '[', ']', '{', '}')
    val quotes = Array(''', '"')
    false
  }

  val testLines = Array(
    "[hello}{]",
    "[Hello]",
    "[Hello{}]()",
    "[(])",
    "Good \"Quotes\" here '{}'",
    "''{'}'"
  )

  testLines.map((t) => t + " -> " + validateBrackets(t)).foreach(println)

}
