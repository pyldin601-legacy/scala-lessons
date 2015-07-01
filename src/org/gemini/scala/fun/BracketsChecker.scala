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
   * @param text String with text to check
   * @return Boolean
   */
  def check(text: String): Boolean = {
    val brackets = Array('(', ')', '[', ']', '{', '}')
    def _val(chars: Array[Char], current: List[Int] = Nil): Boolean =
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
    _val(text.toCharArray)
  }

  def checkDumb(text: String): Boolean = {
    val pairs = Array("()", "{}", "[]")
    def _check(text: String): Boolean = {
      val c = pairs.find(text.contains(_))
      if (text.length == 0) true
      else if (c.isEmpty) false
      else c.forall(str => _check(text.replace(str, "")))
    }
    _check(text)
  }

  def checkMath(text: String): Boolean = {
    val filter: PartialFunction[Char, Int] = {
      case '(' => 0; case ')' => 1
      case '{' => 2; case '}' => 3
      case '[' => 4; case ']' => 5
    }
    val bracketsStream = text.toCharArray collect filter
    false
  }

  val testLines = Array(
//    "a(b)",
//    "[{{}}]",
//    "[(]",
//    "}{",
    "z([{}-()]{a})"
//    ""
  )

  testLines.map((t) => t + " -> " + checkMath(t)).foreach(println)

}
