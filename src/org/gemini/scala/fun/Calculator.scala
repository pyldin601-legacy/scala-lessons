package org.gemini.scala.fun

/**
 * Created by Roman on 08.07.2015
 */

/*

1. Digits
2. Brackets
3. Functions
4. Operator

*/

object Calculator extends App {

  sealed abstract class Token(val priority: Int, val value: Char)

}
