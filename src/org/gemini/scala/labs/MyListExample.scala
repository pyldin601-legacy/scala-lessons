package org.gemini.scala.labs

/**
 * Created by roman on 12.07.15
 */
object MyListExample extends App {

  abstract class MyList[E] {
    def empty: Boolean
    def head: E
    def tail: MyList[E]
  }

  object MyNil extends MyList[Any] {
    def empty = true
    def head = throw new NoSuchElementException
    def tail = throw new NoSuchElementException
  }

  class MyListItem[E](item: E) extends MyList[E] {
    def empty = false
    def head = item
    def tail = ???
  }


}
