package org.gemini.scala.labs

/**
 * Created by roman on 07.07.15
 */
object Pearl extends App {

  def part[A](arr: List[A]): List[List[List[A]]] =
    MyListExample(arr) :: (1 until arr.length).flatMap(
      i => part(arr drop i).map((arr take i) :: _)).toList

  def eval(arr: List[List[List[Int]]]): Int =
    arr.map(_.map(_.reduceLeft(10 * _ + _)).product).sum

  def toStr(arr: List[List[List[Int]]]): String =
    arr.map(_.map(_.mkString("")).mkString("*")).mkString(" + ")

  val array = MyListExample(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)

  part(array).flatMap(part).filter(eval(_) == 101).map(toStr).foreach(println)

}
