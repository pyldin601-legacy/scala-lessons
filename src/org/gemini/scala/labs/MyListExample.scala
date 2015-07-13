package org.gemini.scala.labs

/**
 * Created by roman on 12.07.15
 */
object MyListExample extends App {

  sealed abstract class MyList[+E] {
    def isEmpty: Boolean
    def head: E
    def tail: MyList[E]

    def ::[B >: E](item: B): MyList[B] = {
      new MyListItem[B](item, this)
    }
  }

  object MyNil extends MyList[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException
    def tail: MyList[Nothing] = throw new NoSuchElementException
  }

  case class MyListItem[E](private[scala] val item: E, private[scala] val tl: MyList[E]) extends MyList[E] {
    def isEmpty: Boolean = false
    def head: E = item
    def tail: MyList[E] = tl
  }


  val list = 1 :: 2 :: 3 :: MyNil
  
}
