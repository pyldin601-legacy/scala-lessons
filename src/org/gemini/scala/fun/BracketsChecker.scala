package org.gemini.scala.fun

/**
 * Created by Roman on 24.06.2015
 */
object BracketsChecker extends App {

  /**
   * Function which checks whether brackets within
   * text are correctly nested. It considers brackets
   * of three kinds: (), [], {}.
   * 
   * @param text String with text to checks
   * @return Boolean
   */
  def checkBrackets(text: String): Boolean = {
    val brackets = Array('(', ')', '[', ']', '{', '}')
    def _val(chars: Array[Char], current: List[Int] = Nil): Boolean = {
      chars.isEmpty match {
        case true => current.isEmpty
        case _ =>
          brackets.indexOf(chars.head) match {
            case -1 => _val(chars.tail, current)
            case index =>
              if ((index & 1) == 0) _val(chars.tail, current :+ index)
              else if (current.nonEmpty && index - current.last == 1)
                _val(chars.tail, current.dropRight(1))
              else false
          }
      }
    }
    _val(text.toCharArray)
  }

  val testLines = Array(
    "a(b)",
    "[{}]",
    "[(]",
    "}{",
    "z([{}-()]{a})",
    ""
  )

  testLines.map((t) => t + " -> " + checkBrackets(t)).foreach(println)

}
