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
    def _val(chars: Array[Char], current: Option[Int] = None): Boolean = {
      chars.isEmpty match {
        case true => current.isEmpty
        case _ =>
          val bracketIndex = brackets.indexOf(chars.head)

          if (bracketIndex == -1) _val(chars.tail, current)
          else if (current.isDefined && bracketIndex == current.get) true
          else if ((bracketIndex & 1) == 0) _val(chars.tail, Some(bracketIndex))
          else if (current.isDefined && bracketIndex - current.get == 1) true
          else false
      }

    }
    _val(str.toCharArray)
  }

  val testLines = Array(
    "[hello}{]",
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
