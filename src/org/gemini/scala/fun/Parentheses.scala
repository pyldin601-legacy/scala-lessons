package org.gemini.scala.fun

import scala.collection.mutable

/**
 * Created by Roman on 24.06.2015.
 */
object Parentheses extends App {
  def isValidParentheses(str: String): Boolean = {
    val validOpeners = Array('(', '[', '{')
    val validClosers = Array(')', ']', '}')
    val stack = mutable.Stack[Int]()
    for (c <- str) {
      if (validOpeners contains c) {
        stack.push(validOpeners indexOf c)
      } else if (validClosers contains c) {
        if (stack.isEmpty) {
          return false
        }
        if (validClosers.indexOf(c) != stack.pop()) {
          return false
        }
      }
    }
    true
  }

  println(isValidParentheses("[Hello]"))
  println(isValidParentheses("['Hello'{}]()"))
  println(isValidParentheses("Test[(])"))
  println(isValidParentheses("Test Test"))
  println(isValidParentheses("{{{{}}}}"))
}
